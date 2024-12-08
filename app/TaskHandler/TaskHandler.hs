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

type Response = String -- Replace with actual type
type InstructionData = String -- Replace with actual type
type TaskHistory = String 

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

deleteTaskHandler :: String -> IO (Maybe Response)
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

instructionInputHandler :: InstructionData -> IO (Maybe Response)
instructionInputHandler _ = return $ Just "Instruction Processed" -- Stub

pollTaskHistoryHandler :: IO (Maybe [Task])
pollTaskHistoryHandler = do
  conn <- open "tasks.db"
  tasks <- getAllTasksDB conn
  close conn
  return $ Just tasks

pollWorkspaceUpdateHandler :: String -> IO (Maybe Task)
pollWorkspaceUpdateHandler iden = do
  conn <- open "tasks.db"
  task <- getTaskByIdDB conn iden
  close conn
  return task

