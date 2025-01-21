{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import GHC.Generics
import qualified Network.WebSockets as WS
import System.Process (createProcess, proc)
import System.Environment (getArgs)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Control.Concurrent
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad (void, when)
import Control.Exception (catch, SomeException)
import Database.SQLite.Simple
import Data.Time.Clock (getCurrentTime)
import Data.Text as T hiding (map, filter, any)
import Data.Time (UTCTime)
import Data.Maybe (catMaybes, isNothing)
import Data.List (maximumBy)
import Data.Aeson (decode, encode, FromJSON)
import Control.Monad (forever)
import AIClient.AIClient (Input(..), ServerOutput(..), IterationOutput(..), sendToModel)
import ToolUse.ToolUse 
import ToolUse.GeckoDriver (createSession, getScreenshot, goToURL, resizeViewport)
import WebServer.WebServer (runServer)
import TaskHandler.TaskHandler
import Database.Database

createInput :: UTCTime -> Task -> Maybe [Iteration] -> Input
createInput curTime task maybeIters = case maybeIters of
  Nothing -> Input 
      { iteration = 0
      , ogUserPrompt = taskPrompt task
      , taskCreationTime = taskCreatedAt task
      , curTime = curTime
      , prevOutput = []
      }
  Just iters -> Input 
      { iteration = nextIterNum
      , ogUserPrompt = taskPrompt task
      , taskCreationTime = taskCreatedAt task
      , curTime = curTime
      , prevOutput = formattedOutputs
      }
    where
      maxIter = maximumBy (\i1 i2 -> compare (iterNum i1) (iterNum i2)) iters
      nextIterNum = iterNum maxIter + 1
      maxFormattedOutput = formattedOutput maxIter
      otherServerOutputs = map (serverOutput . formattedOutput) 
                           (filter (\iter -> iterId iter /= iterId maxIter) iters)
      formattedOutputs = maxFormattedOutput : map (`IterationOutput` []) otherServerOutputs

createNewIter :: Task -> Input -> ServerOutput -> [Maybe ToolOutput] -> UTCTime -> Iteration
createNewIter task input serverOut toolOut timestamp = Iteration
  { iterId = 0
  , taskID = taskId task
  , iterNum = iteration input
  , formattedInput = input
  , formattedOutput = IterationOutput serverOut (catMaybes toolOut)
  , iterCreatedAt = timestamp
  }

taskRunner :: (Task, Maybe [Iteration]) -> IO ()
taskRunner (task, iterList) = do
  curTime <- getCurrentTime
  let inputData = createInput curTime task iterList
  response <- sendToModel inputData
  case response of
    Just r -> do
      toolOut <- executeToolUse $ formatCommands r
      newTime <- getCurrentTime
      let iteration = createNewIter task inputData r toolOut newTime
      conn <- open "tasks.db"
      addIterationDB conn iteration
      close conn
      case any isNothing toolOut of
        False -> return ()
        True -> do
          conn <- open "tasks.db"
          updateTaskStatusDB conn (show $ taskID iteration)
          close conn
    Nothing -> return ()

scheduler :: IO ()
scheduler = forever $ do
  conn <- open "tasks.db"
  runningTasks <- getTasksByStatusDB conn "running"
  close conn
  case runningTasks of
    Nothing -> return ()
    Just rt -> do
      mapM_ taskRunner rt

sendImage :: WS.Connection -> T.Text -> IO ()
sendImage conn sessionid = do
  screenshot <- getScreenshot sessionid
  case screenshot of
    Nothing -> return ()
    Just s -> WS.sendBinaryData conn s

-- RL specific stuff here

observationServer :: SharedState -> T.Text -> WS.PendingConnection -> IO ()
observationServer state sid pending = do
  conn <- WS.acceptRequest pending
  loop conn
  where 
    loop conn = do
      signal <- waitForSignal state
      when (signal == "running") $ do
        sendImage conn sid
        loop conn

actionServer :: SharedState -> T.Text -> CachedEnvStatus-> WS.PendingConnection -> IO ()
actionServer state sid cache pending = do
  conn <- WS.acceptRequest pending
  loop conn
  where 
    loop conn = do
      signal <- waitForSignal state
      when (signal == "running") $ do
        action <- WS.receiveData conn
        case decode action :: Maybe BrowserAction of
          Just a -> do
            status <- executeAction sid a cache
            WS.sendTextData conn (encode status)
          Nothing -> WS.sendTextData conn (T.pack "Invalid data format for action")
        loop conn

controlServer :: SharedState -> T.Text -> CachedEnvStatus -> WS.PendingConnection -> IO ()
controlServer state sid cache pending = do
  conn <- WS.acceptRequest pending
  forever $ do
    msg <- WS.receiveData conn
    case decode msg :: Maybe ControlMessage of
      Just a -> do
        case command a of
          "init" -> do
            resetTask sid
            defineTask state (envName a)
            WS.sendTextData conn (T.pack "Task received")
          "reset" -> do
            resetTask sid
            atomically $ writeTVar cache Nothing
            WS.sendTextData conn (T.pack "Reset received")
          "start" -> do
            notifyServers state "running"
            WS.sendTextData conn (T.pack "Start received")
          "stop" -> do
            notifyServers state "idle"
            WS.sendTextData conn (T.pack "Stop received")
          _ -> putStrLn "Invalid Control Message"
      Nothing -> WS.sendTextData conn (T.pack "Invalid data format for Control Server")
            
initializeSharedState :: IO SharedState
initializeSharedState = do
  sig <- newTVarIO "idle"
  tsk <- newTVarIO Nothing
  return $ SharedState sig tsk

notifyServers :: SharedState -> Signal -> IO ()
notifyServers state sig = atomically $ writeTVar (signal state) sig

waitForSignal :: SharedState -> IO Signal
waitForSignal state = atomically $ do
  sig <- readTVar (signal state)
  if sig == "idle"
    then retry -- Block until signal changes
  else return sig

defineTask :: SharedState -> Maybe String -> IO ()
defineTask state tsk = atomically $ do
  writeTVar (task state) tsk

resetTask :: T.Text -> IO ()
resetTask sid = do
  -- This is hardcoded for now since we only have one task
  let site = (T.pack "https://en.wikipedia.org/wiki/Special:Random")
  goToURL sid site

safeRunServer :: String -> Int -> WS.ServerApp -> IO ()
safeRunServer host port app =
  WS.runServer host port app `catch` \(e :: SomeException) -> do
  putStrLn $ "Server on " ++ host ++ ":" ++ show port ++ " failed: " ++ show e

-- TODO: data Signal = Running | Idle deriving (Show, Eq)
type Signal = String -- Possible values: "running", "idle"
data SharedState = SharedState
  { signal :: TVar Signal
  , task   :: TVar (Maybe String)
  }

data ControlMessage = ControlMessage
  { command :: String
  , envName :: Maybe String
  } deriving (Show, Generic)

instance FromJSON ControlMessage

main :: IO ()
main = do 
  args <- getArgs
  let rl = "rl" `Prelude.elem` args
  case rl of 
    False -> do
      conn <- open "tasks.db"
      initializeDatabase conn
      close conn
      _ <- forkIO scheduler
      runServer 8080
    True -> do
      state <- initializeSharedState
      cachedStatus <- newTVarIO Nothing  -- Cached state for reward calculation in actionServer
      seshId <- createSession
      case seshId of
        Nothing -> error "Unable to create browser session in RL mode"
        Just sid -> do
          resizeViewport sid 1080 1920
          putStrLn "Starting servers..."
          done <- newMVar 3
          _ <- forkIO (safeRunServer "localhost" 8764 (controlServer state sid cachedStatus) >> putMVar done 1)
          _ <- forkIO (safeRunServer "localhost" 8765 (observationServer state sid) >> putMVar done 1)
          _ <- forkIO (safeRunServer "localhost" 8766 (actionServer state sid cachedStatus) >> putMVar done 1)
          takeMVar done
          takeMVar done
          takeMVar done
          return ()
