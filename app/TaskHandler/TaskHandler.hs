{-# LANGUAGE DeriveGeneric #-}

module TaskHandler.TaskHandler
  ( newTaskHandler
  , deleteTaskHandler
  , updateTaskStatusHandler
  , instructionInputHandler
  , pollTaskHistoryHandler
  , pollWorkspaceUpdateHandler
  , TaskData(..)
  ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Database.Database
import Database.SQLite.Simple

data TaskData = TaskData
  { taskName  :: String
  , modelType :: String
  } deriving (Show, Generic)

instance FromJSON TaskData
instance ToJSON TaskData

newTaskHandler :: TaskData -> IO (Maybe String)
newTaskHandler task = do
  conn <- open "tasks.db"
  name <- addTaskDB conn (taskName task) (modelType task)
  close conn
  return $ Just $ "Created task: " ++ name

deleteTaskHandler :: String -> IO (Maybe String)
deleteTaskHandler iden = do
  conn <- open "tasks.db"
  name <- deleteTaskDB conn iden
  close conn
  return $ Just "Task Deleted" 

updateTaskStatusHandler :: String -> IO (Maybe String)
updateTaskStatusHandler iden = do
  conn <- open "tasks.db"
  newStatus <- updateTaskStatusDB conn iden
  close conn
  return $ newStatus

instructionInputHandler :: String -> String -> IO (Maybe String)
instructionInputHandler iden inst = do
  conn <- open "tasks.db"
  name <- instructionInputDB conn iden inst
  close conn
  return $ Just "Instruction Added"

pollTaskHistoryHandler :: IO (Maybe [Task])
pollTaskHistoryHandler = do
  conn <- open "tasks.db"
  tasks <- getAllTasksDB conn
  close conn
  return $ Just tasks

pollWorkspaceUpdateHandler :: String -> IO (Maybe [Iteration])
pollWorkspaceUpdateHandler iden = do
  conn <- open "tasks.db"
  iters <- getIterationByIdDB conn iden
  close conn
  return iters

