module Main where

import System.Process (createProcess, proc)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Control.Concurrent
import Database.SQLite.Simple
import Data.Time.Clock (getCurrentTime)
import Data.Time (UTCTime)
import Data.Maybe (catMaybes, isNothing)
import Data.List (maximumBy)
import Control.Monad (forever)
import AIClient.AIClient (Input(..), ServerOutput(..), IterationOutput(..), sendToModel)
import ToolUse.ToolUse 
import WebServer.WebServer (runServer)
import TaskHandler.TaskHandler
import Database.Database

createInput :: UTCTime -> Task -> Maybe [Iteration] -> Input
createInput curTime task maybeIters = case maybeIters of
  Nothing -> Input 
      { iteration = 0
      , ogUserPrompt = taskPrompt task
      , taskCreationTime = taskCreatedAt task
      , curTime = curTime
      , prevOutput = []
      }
  Just iters -> Input 
      { iteration = nextIterNum
      , ogUserPrompt = taskPrompt task
      , taskCreationTime = taskCreatedAt task
      , curTime = curTime
      , prevOutput = formattedOutputs
      }
    where
      maxIter = maximumBy (\i1 i2 -> compare (iterNum i1) (iterNum i2)) iters
      nextIterNum = iterNum maxIter + 1
      maxFormattedOutput = formattedOutput maxIter
      otherServerOutputs = map (serverOutput . formattedOutput) 
                           (filter (\iter -> iterId iter /= iterId maxIter) iters)
      formattedOutputs = maxFormattedOutput : map (`IterationOutput` []) otherServerOutputs

createNewIter :: Task -> Input -> ServerOutput -> [Maybe ToolOutput] -> UTCTime -> Iteration
createNewIter task input serverOut toolOut timestamp = Iteration
  { iterId = 0
  , taskID = taskId task
  , iterNum = iteration input
  , formattedInput = input
  , formattedOutput = IterationOutput serverOut (catMaybes toolOut)
  , iterCreatedAt = timestamp
  }

taskRunner :: (Task, Maybe [Iteration]) -> IO ()
taskRunner (task, iterList) = do
  curTime <- getCurrentTime
  let inputData = createInput curTime task iterList
  response <- sendToModel inputData
  case response of
    Just r -> do
      toolOut <- executeToolUse $ formatCommands r
      newTime <- getCurrentTime
      let iteration = createNewIter task inputData r toolOut newTime
      conn <- open "tasks.db"
      addIterationDB conn iteration
      close conn
      case any isNothing toolOut of
        False -> return ()
        True -> do
          conn <- open "tasks.db"
          updateTaskStatusDB conn (show $ taskID iteration)
          close conn
    Nothing -> return ()

scheduler :: IO ()
scheduler = forever $ do
  conn <- open "tasks.db"
  runningTasks <- getTasksByStatusDB conn "running"
  close conn
  case runningTasks of
    Nothing -> return ()
    Just rt -> do
      mapM_ taskRunner rt

main :: IO ()
main = do 
  conn <- open "tasks.db"
  initializeDatabase conn
  close conn
  _ <- forkIO scheduler
  runServer 8080
