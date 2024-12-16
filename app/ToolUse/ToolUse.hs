{-# Language OverloadedStrings #-}

module ToolUse.ToolUse 
  ( executeToolUse
  ) where

import AIClient.AIClient (Command(..))
import ToolUse.GeckoDriver 

data SearchOutput = SearchOutput 
  { query        :: String
  , searchSrc    :: Maybe String
  , searchImg    :: Maybe String
  } deriving (Show)

data BrowseOutput = BrowseOutput
  { url          :: String
  , browseSrc    :: Maybe String
  , browseImg    :: Maybe String
  } deriving (Show)

data ToolOutput = ToolOutput
  { searchOut :: Maybe SearchOutput
  , browseOut :: Maybe BrowseOutput
  } deriving (Show)

search :: String -> IO SearchOutput
search query = undefined

browse :: String -> IO BrowseOutput
browse url = do
  session_id <- createSession
  goToURL session_id url
  pageSrc <- getPageSource session_id
  screenshot <- getScreenshot session_id
  return $ BrowseOutput url pageSrc screenshot

runCommand :: Command -> ToolOutput
runCommand cmd = 
  case action cmd of
    "search" -> ToolOutput (Just $ search (arg cmd)) Nothing
    "browse" -> ToolOutput Nothing (Just $ browse (arg cmd))
    _        -> ToolOutput Nothing Nothing

executeToolUse :: [Command] -> IO [ToolOutput]
executeToolUse cmds = return $ map runCommand cmds
