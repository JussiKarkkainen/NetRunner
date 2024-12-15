module Main where

import Database.SQLite.Simple
import Data.Time.Clock (getCurrentTime)
import Data.Time (UTCTime)
import Data.List (maximumBy)
import Control.Monad (forever)
import AIClient.AIClient (Input(..), Output(..), sendToModel)
import ToolUse.ToolUse 
import WebServer.WebServer (runServer)
import TaskHandler.TaskHandler
import Database.Database
import Control.Concurrent

createInput :: UTCTime -> Task -> Maybe [Iteration] -> Input
createInput curTime t mis = case mis of
  Nothing -> Input 0 (taskPrompt t) (taskCreatedAt t) curTime Nothing
  Just is -> Input newIterNum (taskPrompt t) (taskCreatedAt t) curTime prevIn
    where maxIter = maximumBy (\i1 i2 -> compare (iterNum i1) (iterNum i2)) is
          newIterNum = iterNum maxIter + 1
          prevIn = prevInput (formattedInput maxIter)

-- Type should be (Task, Maybe [Iteration]) -> IO (Iteration)
taskRunner :: (Task, Maybe [Iteration]) -> IO ()
taskRunner (task, iterList) = do
  curTime <- getCurrentTime
  let inputData = createInput curTime task iterList
  response <- sendToModel inputData
  case response of
    Just r -> do
      print r
      error "Stop"
      -- toolOut <- executeToolUse r
      -- return $ createNewIter response toolOut
      return ()
    Nothing -> return ()
  return ()

scheduler :: IO ()
scheduler = forever $ do
  conn <- open "tasks.db"
  runningTasks <- getTasksByStatusDB conn "running"
  close conn
  case runningTasks of
    Nothing -> return ()
    Just rt -> do
      _ <- traverse taskRunner rt
      return ()

main :: IO ()
main = do 
  conn <- open "tasks.db"
  initializeDatabase conn
  close conn
  _ <- forkIO scheduler
  runServer 8080
