module Main where

import Database.SQLite.Simple
import WebServer.WebServer (runServer)
import TaskHandler.TaskHandler
import Database.Database

debug = False

main :: IO ()
main = do 
  conn <- open "tasks.db"
  initializeDatabase conn
  case debug of
    True -> do
      addTask conn "debugtaskname3" "debug-claude"
      tasks <- getAllTasks conn
      mapM_ print tasks
      close conn
    False -> do
      runServer 8080

