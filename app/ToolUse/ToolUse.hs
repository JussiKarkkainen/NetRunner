module ToolUse.ToolUse 
  ( executeToolUse
  ) where

import AIClient.AIClient (Command(..))


data SearchOutput = SearchOutput 
  { query        :: String
  , searchResult :: String
  } deriving (Show)

data BrowseOutput = BrowseOutput
  { url          :: String
  , browseResult :: String
  } deriving (Show)

data ToolOutput = ToolOutput
  { tool       :: String
  , arg        :: String
  , toolResult :: String
  } deriving (Show)

search :: String -> IO SearchOutput
search query = undefined

browse :: String -> IO BrowseOutput
browse url = undefined

executeToolUse :: [Command] -> IO ToolOutput
executeToolUse = undefined
