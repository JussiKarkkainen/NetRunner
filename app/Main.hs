module Main where

import Database.SQLite.Simple
import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Clock (getCurrentTime)
import AIClient.AIClient
import ToolUse.ToolUse
import WebServer.WebServer (runServer)
import TaskHandler.TaskHandler
import Database.Database
import Control.Concurrent

data Input = Input 
  { iteration :: Int
  , ogUserPrompt :: String
  , taskCreationTime :: UTCTime
  , curTime :: UTCTime
  , prevInput :: Maybe String
  } deriving (Show, Generic)

instance FromJSON Input
instance ToJSON Input

createInput :: UTCTime -> Task -> Maybe [Iteration] -> Input
createInput curTime t mis = case mis of
  Nothing -> Input 0 (taskPrompt t) (taskCreatedAt t) curTime Nothing
  Just is -> Input newIterNum (taskPrompt t) (taskCreatedAt t) curTime prevIn
    where maxIter = maximumBy (\i1 i2 -> compare (iterNum i1) (iterNum i2)) is
          newIterNum = iterNum maxIter + 1
          prevIn = formattedInput maxIter

taskRunner :: Task -> Iteration
taskRunner task = undefined 

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
