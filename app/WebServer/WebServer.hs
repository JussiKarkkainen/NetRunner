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
import TaskHandler.TaskHandler 
                              

runServer :: Int -> IO ()
runServer port = scotty port $ do
  get "/" mainPage
  post "/new_task" newTask
  post "/delete_task" deleteTask
  post "/update_task_status" updateTaskStatus
  post "/instruction_input" instructionInput
  post "/poll_workspace_update" pollWorkspaceUpdate
  post "/poll_task_meta" pollTaskMeta
  get "/poll_task_history" pollTaskHistory
  get "/static/styles.css" fetchCSS

mainPage :: ActionM ()
mainPage = do
  setHeader "Content-Type" "text/html"
  file "static/index.html"

newTask :: ActionM ()
newTask = do
  taskData <- jsonData 
  maybeResponse <- liftIO $ newTaskHandler taskData
  case maybeResponse of 
    Nothing -> do
      status internalServerError500
      json $ object ["error" .= ("Failed to create task" :: String)]
    Just response -> json response

data Instruction = Instruction { instruction :: String, identask :: String} deriving (Show, Generic)
instance FromJSON Instruction
instance ToJSON Instruction

instructionInput :: ActionM ()
instructionInput = do
  instructionData <- jsonData :: ActionM Instruction
  let idValue = identask instructionData
  let inst = instruction instructionData
  maybeResponse <- liftIO $ instructionInputHandler idValue inst
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

updateTaskStatus :: ActionM ()
updateTaskStatus = do
  taskId <- jsonData :: ActionM TaskId
  let taskIdValue = idtask taskId
  maybeResponse <- liftIO $ updateTaskStatusHandler taskIdValue
  case maybeResponse of
    Nothing -> do
      status internalServerError500
      json $ object ["error" .= ("Failed to start task" :: String)]
    Just response -> json response

deleteTask :: ActionM ()
deleteTask = do
  taskId <- jsonData :: ActionM TaskId
  let taskIdValue = idtask taskId
  maybeResponse <- liftIO $ deleteTaskHandler taskIdValue
  case maybeResponse of 
    Nothing -> do
      status internalServerError500
      json $ object ["error" .= ("Failed to delete task" :: String)]
    Just response -> json response

pollTaskMeta :: ActionM ()
pollTaskMeta = do
  taskId <- jsonData :: ActionM TaskId
  let taskIdValue = idtask taskId
  maybeTask <- liftIO $ pollTaskMetaHandler taskIdValue
  case maybeTask of
    Nothing -> do
      status internalServerError500
      json $ object ["error" .= ("Failed to poll task meta update" :: String)]
    Just t -> json t

pollWorkspaceUpdate :: ActionM ()
pollWorkspaceUpdate = do
  taskId <- jsonData :: ActionM TaskId
  let taskIdValue = idtask taskId
  maybeIters <- liftIO $ pollWorkspaceUpdateHandler taskIdValue
  case maybeIters of
    Nothing -> do
      status internalServerError500
      json $ object ["error" .= ("Failed to poll workspace update" :: String)]
    Just iters -> json iters

fetchCSS :: ActionM ()
fetchCSS = do
  css <- liftIO $ TL.readFile "static/styles.css"
  setHeader "Content-Type" "text/css"
  text css

