module ToolUse.ToolUse 
  ( executeToolUse
  ) where

import AIClient.AIClient (Command(..))


search :: String -> SearchOutput
search query = undefined

browse :: String -> BrowseOutput
browse url = undefined

executeToolUse :: [Command] -> ToolOutput
executeToolUse = undefined
