{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.Database
  ( addTaskDB
  , getAllTasksDB
  , getTaskByIdDB
  , getTasksByStatusDB
  , instructionInputDB
  , updateTaskStatusDB
  , deleteTaskDB
  , addIterationDB
  , initializeDatabase
  , Task(..)
  , TaskStatus(..)
  , Iteration(..)
  ) where

import Database.SQLite.Simple
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.FromField (FromField (fromField), returnError)
import Data.Time (UTCTime, getCurrentTime)
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import qualified Data.ByteString.Lazy as BSL
import GHC.Generics (Generic)
import AIClient.AIClient (Input(..), Output(..))

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

data Iteration = Iteration
  { iterId          :: Int
  , taskID          :: Int
  , iterNum         :: Int
  , formattedInput  :: Input
  , formattedOutput :: Output
  , iterCreatedAt   :: UTCTime
  } deriving (Show, Generic)

instance FromJSON Iteration
instance ToJSON Iteration

initializeDatabase :: Connection -> IO ()
initializeDatabase conn = do
  execute_ conn
    "CREATE TABLE IF NOT EXISTS tasks (\
    \id INTEGER PRIMARY KEY AUTOINCREMENT,\
    \name TEXT NOT NULL,\
    \model_type TEXT NOT NULL,\
    \status TEXT NOT NULL DEFAULT 'pending',\
    \user_prompt TEXT NOT NULL DEFAULT 'none',\
    \created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP)"

  execute_ conn
    "CREATE TABLE IF NOT EXISTS iterations (\
    \iterid INTEGER PRIMARY KEY AUTOINCREMENT,\
    \taskid INTEGER NOT NULL,\
    \iternum INTEGER NOT NULL,\
    \input STRING NOT NULL,\
    \output STRING NOT NULL,\
    \created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP)"
  putStrLn "Database initialized."

instance FromRow Task where
  fromRow = Task <$> field <*> field <*> field <*> field <*> field <*> field

instance FromRow Iteration where
  fromRow = do
    iid <- field       -- iterid
    tid <- field       -- taskid
    num <- field       -- iternum
    inputStr <- field  -- input string (JSON)
    outputStr <- field -- output string (JSON)
    ts <- field        -- created_at
    case (decode (BSL.fromStrict inputStr), decode (BSL.fromStrict outputStr)) of
        (Just input, Just output) -> 
            return $ Iteration iid tid num input output ts
        _ -> error "Failed to decode input/output JSON"

addTaskDB :: Connection -> String -> String -> IO String
addTaskDB conn name modelType = do
  currentTime <- getCurrentTime
  execute conn
    "INSERT INTO tasks (name, model_type, created_at) VALUES (?,?,?)"
    (name, modelType, currentTime)
  return name

addIterationDB :: Connection -> Iteration -> IO ()
addIterationDB conn iter = do
  execute conn 
    "INSERT INTO iterations (taskid, iternum, input, output, created_at) VALUES (?,?,?,?,?)"
    (taskID iter, iterNum iter, BSL.toStrict $ encode (formattedInput iter), BSL.toStrict $ encode (formattedOutput iter), iterCreatedAt iter)      

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

instructionInputDB :: Connection -> String -> String -> IO String
instructionInputDB conn iden inst = do
  execute conn "UPDATE tasks SET user_prompt = ? WHERE id = ?" (inst, iden)
  return $ "Added Instruction: " ++ inst

getAllTasksDB :: Connection -> IO [Task]
getAllTasksDB conn = do
  tasks <- query_ conn "SELECT * FROM tasks" :: IO [Task]
  return tasks

getTaskByIdDB :: Connection -> String -> IO (Maybe Task)
getTaskByIdDB conn iden = do
  task <- queryNamed conn "SELECT * FROM tasks WHERE id = :id" [":id" := iden]
  return $ case task of
    [task] -> Just task
    _      -> Nothing

getIterationByIdDB :: Connection -> Int -> IO (Maybe [Iteration])
getIterationByIdDB conn taskid = do
  iters <- queryNamed conn "SELECT * FROM iterations WHERE taskid = :taskid" [":taskid" := taskid]
  return $ case iters of
    [] -> Nothing
    _  -> Just iters
  
getTasksByStatusDB :: Connection -> String -> IO (Maybe [(Task, Maybe [Iteration])])
getTasksByStatusDB conn stat = do
  tasks <- queryNamed conn "SELECT * FROM tasks WHERE status = :status" [":status" := stat] :: IO [Task]
  if null tasks 
    then return Nothing
    else do
      taskIters <- mapM (\task -> do
        iters <- getIterationByIdDB conn (taskId task)
        return (task, iters)) tasks
      return $ Just taskIters
