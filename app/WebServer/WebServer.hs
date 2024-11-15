{-# LANGUAGE OverloadedStrings #-}

module WebServer.WebServer
  ( runServer
  ) where

import Web.Scotty
import Data.Aeson (object, (.=), FromJSON, ToJSON)
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types.Status (notFound404, internalServerError500)
import TaskHandler.TaskHandler (newTaskHandler, deleteTaskHandler , startTaskHandler, stopTaskHandler,
                                instructionInputHandler, pollWorkspaceUpdateHandler, TaskData)

runServer :: Int -> IO ()
runServer port = scotty port $ do
  get "/" mainPage
  post "/new_task" newTask
  post "/delete_task" deleteTask
  post "/start_task" startTask
  post "/stop_task" stopTask
  post "/instruction_input" instructionInput
  get "/poll_workspace_update" pollWorkspaceUpdate

mainPage :: ActionM ()
mainPage = do
  setHeader "Content-Type" "text/html"
  file "static/index.html"

newTask :: ActionM ()
newTask = do
  taskData <- jsonData 
  maybeTask <- liftIO $ newTaskHandler taskData
  case maybeTask of 
    Nothing -> do
      status internalServerError500
      json $ object ["error" .= ("Failed to create task" :: String)]
    Just task -> do
      json task  


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

pollWorkspaceUpdate :: ActionM ()
pollWorkspaceUpdate = do
  taskId <- param "task_name"
  maybeUpdate <- liftIO $ pollWorkspaceUpdateHandler taskId
  case maybeUpdate of
    Nothing -> do
      status internalServerError500
      json $ object ["error" .= ("Failed to poll workspace update" :: String)]
    Just update -> json update
