{-# LANGUAGE DeriveGeneric #-}

module TaskHandler.TaskHandler
  ( newTaskHandler
  , deleteTaskHandler
  , startTaskHandler
  , stopTaskHandler
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

newTaskHandler :: TaskData -> IO String
newTaskHandler task = do
  conn <- open "tasks.db"
  name <- addTask conn (taskName task) (modelType task)
  return $ "Created task: " ++ name

deleteTaskHandler :: TaskData -> IO (Maybe Response)
deleteTaskHandler _ = return $ Just "Task Deleted" -- Stub

startTaskHandler :: TaskData -> IO (Maybe Response)
startTaskHandler _ = return $ Just "Task Started" -- Stub

stopTaskHandler :: TaskData -> IO (Maybe Response)
stopTaskHandler _ = return $ Just "Task Stopped" -- Stub

instructionInputHandler :: InstructionData -> IO (Maybe Response)
instructionInputHandler _ = return $ Just "Instruction Processed" -- Stub

pollTaskHistoryHandler :: IO (Maybe [Task])
pollTaskHistoryHandler = do
  conn <- open "tasks.db"
  tasks <- getAllTasks conn
  close conn
  return $ Just tasks

pollWorkspaceUpdateHandler :: String -> IO (Maybe Task)
pollWorkspaceUpdateHandler id = do
  conn <- open "tasks.db"
  task <- getTaskById conn id
  close conn
  return task

