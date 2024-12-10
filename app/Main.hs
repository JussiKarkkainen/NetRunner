module Main where

import Database.SQLite.Simple
import WebServer.WebServer (runServer)
import TaskHandler.TaskHandler
import Database.Database
import Control.Concurrent

taskRunner :: Task -> Task
taskRunner task = 

scheduler :: IO ()
scheduler = forever $ do
  conn <- open "tasks.db"
  runningTasks <- getTasksByStatusDB conn "running"
  close conn
  fmap (map taskRunner) runningTasks

main :: IO ()
main = do 
  conn <- open "tasks.db"
  initializeDatabase conn
  close conn
  _ <- forkIO scheduler
  runServer 8080
