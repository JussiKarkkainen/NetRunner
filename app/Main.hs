module Main where

import System.Process (createProcess, proc)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Control.Concurrent
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

createInput :: UTCTime -> Task -> Maybe [Iteration] -> Input
createInput curTime t mis = case mis of
  Nothing -> Input 0 (taskPrompt t) (taskCreatedAt t) curTime Nothing
  Just is -> Input newIterNum (taskPrompt t) (taskCreatedAt t) curTime prevIn
    where maxIter = maximumBy (\i1 i2 -> compare (iterNum i1) (iterNum i2)) is
          newIterNum = iterNum maxIter + 1
          prevIn = prevInput (formattedInput maxIter)

createNewIter :: Task -> Input -> Output -> ToolOutput -> Iteration
createNewIter t i o to = Iteration null (taskId t) (iteration i) i o null

taskRunner :: (Task, Maybe [Iteration]) -> IO ()
taskRunner (task, iterList) = do
  curTime <- getCurrentTime
  let inputData = createInput curTime task iterList
  response <- sendToModel inputData
  case response of
    Just r -> do
      print r
      toolOut <- executeToolUse $ formatCommands r
      let iteration = createNewIter task inputData r toolOut
      conn <- open "tasks.db"
      addIterationDB conn iteration
      close conn
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

startGeckoDriver :: IO ()
startGeckoDriver = do
  currentDir <- getCurrentDirectory
  let relativePath = "gecko" </> "geckodriver"
  let fullPath = currentDir </> relativePath
  _ <- createProcess (proc fullPath [])
  putStrLn "GeckoDriver started" 

main :: IO ()
main = do 
  conn <- open "tasks.db"
  initializeDatabase conn
  close conn
  startGeckoDriver
  _ <- forkIO scheduler
  runServer 8080
