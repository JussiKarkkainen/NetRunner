{-# Language OverloadedStrings #-}

module ToolUse.ToolUse 
  ( executeToolUse
  ) where

import AIClient.AIClient (Command(..))
import qualified Data.ByteString.Lazy as BL
import ToolUse.GeckoDriver 
import Data.Text as T
import Control.Concurrent (threadDelay)

data SearchOutput = SearchOutput 
  { query        :: String
  , searchSrc    :: Maybe String
  , searchImg    :: Maybe BL.ByteString
  } deriving (Show)

data BrowseOutput = BrowseOutput
  { url          :: String
  , browseSrc    :: Maybe String
  , browseImg    :: Maybe BL.ByteString
  } deriving (Show)

data ToolOutput = ToolOutput
  { searchOut :: Maybe SearchOutput
  , browseOut :: Maybe BrowseOutput
  } deriving (Show)

search :: String -> IO SearchOutput
search query = do
  sessionIdM <- createSession
  case sessionIdM of
    Nothing -> error "Unable to create browser session" 
    Just sessionId -> do
      goToURL sessionId "https://www.google.com"
      elementIdM <- findElement sessionId "name" "q"
      case elementIdM of
        Nothing -> error "Search bar not found"
        Just elementId -> do
          sendKeys sessionId elementId (T.pack query)
          submitElement sessionId elementId
          threadDelay 5000000 -- TODO: Find a better way to wait for search results
          pageSrc <- getPageSource sessionId
          screenshot <- getScreenshot sessionId
          return $ SearchOutput query (T.unpack <$> pageSrc) screenshot

browse :: String -> IO BrowseOutput
browse url = do
  session_id <- createSession
  case session_id of 
    Just sid -> do
      goToURL sid (T.pack url)
      pageSrc <- getPageSource sid
      screenshot <- getScreenshot sid
      return $ BrowseOutput url (T.unpack <$> pageSrc) screenshot
    Nothing -> error "Unable to start browser session"

runCommand :: Command -> IO ToolOutput
runCommand cmd = 
  case action cmd of
    "search" -> do
      searchRes <- search (arg cmd)
      return $ ToolOutput (Just searchRes) Nothing
    "browse" -> do
      browseRes <- browse (arg cmd)
      return $ ToolOutput Nothing (Just browseRes)
    _        -> return $ ToolOutput Nothing Nothing

executeToolUse :: [Command] -> IO [ToolOutput]
executeToolUse cmds = mapM runCommand cmds
