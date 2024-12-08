{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.Database
  ( addTaskDB
  , getAllTasksDB
  , getTaskByIdDB
  , updateTaskStatusDB
  , deleteTaskDB
  , initializeDatabase
  , Task(..)
  , TaskStatus(..)
  ) where

import Database.SQLite.Simple
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.FromField (FromField (fromField), returnError)
import Data.Time (UTCTime, getCurrentTime)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data TaskStatus = Running | Stopped | Finished | Pending | Undefined deriving (Show, Eq, Generic)

instance ToJSON TaskStatus
instance FromJSON TaskStatus

stringToStatus :: String -> TaskStatus
stringToStatus "running" = Running
stringToStatus "stopped" = Stopped
stringToStatus "pending" = Pending

instance FromField TaskStatus where
  fromField fieldData = do
    value <- fromField fieldData :: Ok String
    case value of 
      "running" -> Ok Running
      "stopped" -> Ok Stopped
      "pending" -> Ok Pending
      _ -> returnError ConversionFailed fieldData ("Invalid Status: " ++ value)

data Task = Task 
  { taskId          :: Int
  , taskTitle       :: String
  , taskModelType   :: String
  , taskStatus      :: TaskStatus
  , taskPrompt      :: String
  , taskCreatedAt   :: UTCTime
  } deriving (Show, Generic)

instance FromJSON Task
instance ToJSON Task

initializeDatabase :: Connection -> IO ()
initializeDatabase conn = do
  execute_ conn
    "CREATE TABLE IF NOT EXISTS tasks (\
    \id INTEGER PRIMARY KEY AUTOINCREMENT,\
    \name TEXT NOT NULL,\
    \model_type TEXT NOT NULL,\
    \status TEXT NOT NULL DEFAULT 'pending',\
    \system_prompt TEXT NOT NULL DEFAULT 'none',\
    \created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP)"
  putStrLn "Database initialized."

instance FromRow Task where
  fromRow = Task <$> field <*> field <*> field <*> field <*> field <*> field

addTaskDB :: Connection -> String -> String -> IO String
addTaskDB conn name modelType = do
  currentTime <- getCurrentTime
  execute conn
    "INSERT INTO tasks (name, model_type, created_at) VALUES (?,?,?)"
    (name, modelType, currentTime)
  return name

deleteTaskDB :: Connection -> String -> IO String
deleteTaskDB conn iden = do
  execute conn "DELETE FROM tasks WHERE id = ?" (Only iden)
  return $ "Deleted: " ++ iden

updateTaskStatusDB :: Connection -> String -> IO (Maybe String)
updateTaskStatusDB conn iden = do
  curStatus <- queryNamed conn "SELECT status from tasks WHERE id = :id" [":id" := iden] :: IO [Only String]
  case curStatus of
    [Only stat] -> do
      let newStatus = if stat == "pending" || stat == "stopped" then "running" else "stopped" 
      execute conn "UPDATE tasks SET status = ? WHERE id = ?" (newStatus, iden) 
      return $ Just newStatus
    _     -> return Nothing

getAllTasksDB :: Connection -> IO [Task]
getAllTasksDB conn = do
  tasks <- query_ conn "SELECT id, name, model_type, status, system_prompt, created_at FROM tasks" :: IO [Task]
  return tasks

getTaskByIdDB :: Connection -> String -> IO (Maybe Task)
getTaskByIdDB conn iden = do
  task <- queryNamed conn "SELECT * FROM tasks WHERE id = :id" [":id" := iden]
  return $ case task of
    [task] -> Just task
    _      -> Nothing

