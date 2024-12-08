{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.Database
  ( addTask
  , getAllTasks
  , getTaskById
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

data TaskStatus = Running | Paused | Finished | Pending | Undefined deriving (Show, Eq, Generic)

instance ToJSON TaskStatus
instance FromJSON TaskStatus

stringToStatus :: String -> TaskStatus
stringToStatus "running" = Running
stringToStatus "paused" = Paused
stringToStatus "finished" = Finished
stringToStatus "pending" = Pending
stringToStatus "undefined" = Undefined

instance FromField TaskStatus where
  fromField fieldData = do
    value <- fromField fieldData :: Ok String
    case value of 
      "running" -> Ok Running
      "paused" -> Ok Paused
      "finished" -> Ok Finished
      "pending" -> Ok Pending
      "undefined" -> Ok Undefined
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

addTask :: Connection -> String -> String -> IO String
addTask conn name modelType = do
  currentTime <- getCurrentTime
  execute conn
    "INSERT INTO tasks (name, model_type, created_at) VALUES (?,?,?)"
    (name, modelType, currentTime)
  return name

instance FromRow Task where
  fromRow = Task <$> field <*> field <*> field <*> field <*> field <*> field

getAllTasks :: Connection -> IO [Task]
getAllTasks conn = do
  tasks <- query_ conn "SELECT id, name, model_type, status, system_prompt, created_at FROM tasks" :: IO [Task]
  return tasks

getTaskById :: Connection -> String -> IO (Maybe Task)
getTaskById conn id = do
  task <- queryNamed conn "SELECT * FROM tasks WHERE id = :id" [":id" := id]
  return $ case task of
    [task] -> Just task
    _      -> Nothing

