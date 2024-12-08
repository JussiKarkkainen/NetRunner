{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module WebServer.WebServer
  ( runServer
  ) where

import Web.Scotty
import GHC.Generics (Generic)
import qualified Data.Text.Lazy.IO as TL
import Data.Aeson (object, (.=), FromJSON, ToJSON)
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types.Status (notFound404, internalServerError500)
import TaskHandler.TaskHandler (newTaskHandler, deleteTaskHandler , startTaskHandler, stopTaskHandler,
                                instructionInputHandler, pollTaskHistoryHandler, pollWorkspaceUpdateHandler, TaskData)

runServer :: Int -> IO ()
runServer port = scotty port $ do
  get "/" mainPage
  post "/new_task" newTask
  post "/delete_task" deleteTask
  post "/start_task" startTask
  post "/stop_task" stopTask
  post "/instruction_input" instructionInput
  post "/poll_workspace_update" pollWorkspaceUpdate
  get "/poll_task_history" pollTaskHistory
  get "/static/styles.css" fetchCSS

mainPage :: ActionM ()
mainPage = do
  setHeader "Content-Type" "text/html"
  file "static/index.html"

newTask :: ActionM ()
newTask = do
  taskData <- jsonData 
  name <- liftIO $ newTaskHandler taskData
  json name

deleteTask :: ActionM ()
deleteTask = do
  taskData <- jsonData
  maybeResponse <- liftIO $ deleteTaskHandler taskData
  case maybeResponse of 
    Nothing -> do
      status internalServerError500
      json $ object ["error" .= ("Failed to delete task" :: String)]
    Just response -> json response

startTask :: ActionM ()
startTask = do
  taskData <- jsonData
  maybeResponse <- liftIO $ startTaskHandler taskData
  case maybeResponse of
    Nothing -> do
      status internalServerError500
      json $ object ["error" .= ("Failed to start task" :: String)]
    Just response -> json response

stopTask :: ActionM ()
stopTask = do
  taskData <- jsonData
  maybeResponse <- liftIO $ stopTaskHandler taskData
  case maybeResponse of
    Nothing -> do
      status internalServerError500
      json $ object ["error" .= ("Failed to stop task" :: String)]
    Just response -> json response

instructionInput :: ActionM ()
instructionInput = do
  instructionData <- jsonData
  maybeResponse <- liftIO $ instructionInputHandler instructionData
  case maybeResponse of
    Nothing -> do
      status internalServerError500
      json $ object ["error" .= ("Failed to process instruction input" :: String)]
    Just response -> json response

pollTaskHistory :: ActionM ()
pollTaskHistory = do
  maybeHistory <- liftIO $ pollTaskHistoryHandler
  case maybeHistory of
    Nothing -> do
      status internalServerError500
      json $ object ["error" .= ("Failed to poll task history update" :: String)]
    Just update -> json update

data TaskId = TaskId { idtask :: String } deriving (Show, Generic)
instance FromJSON TaskId
instance ToJSON TaskId

pollWorkspaceUpdate :: ActionM ()
pollWorkspaceUpdate = do
  taskId <- jsonData :: ActionM TaskId
  let taskIdValue = idtask taskId
  maybeTask <- liftIO $ pollWorkspaceUpdateHandler taskIdValue
  case maybeTask of
    Nothing -> do
      status internalServerError500
      json $ object ["error" .= ("Failed to poll workspace update" :: String)]
    Just task -> json task

fetchCSS :: ActionM ()
fetchCSS = do
  css <- liftIO $ TL.readFile "static/styles.css"
  setHeader "Content-Type" "text/css"
  text css

