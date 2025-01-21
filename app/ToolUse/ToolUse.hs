{-# Language OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ToolUse.ToolUse 
  ( executeToolUse
  , executeAction
  , Command(..)
  , ToolOutput(..)
  , BrowserAction(..)
  , RLStatus(..)
  , EnvStatus(..)
  , CachedEnvStatus
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text.Encoding as TE
import Control.Concurrent.STM
import Data.Aeson.Types (object, withObject, Parser)
import Data.Maybe (fromMaybe)
import ToolUse.GeckoDriver 
import ToolUse.HtmlParser (cleanHtml)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), (.:?), fieldLabelModifier, 
                   genericToJSON, defaultOptions, genericParseJSON, camelTo2, decode,
                   eitherDecode)
import GHC.Generics
import Data.Text as T
import Control.Concurrent (threadDelay)

data SearchOutput = SearchOutput 
  { query        :: String
  , searchSrc    :: Maybe String
  , searchImg    :: Maybe BL.ByteString
  } deriving (Show, Generic)

instance ToJSON SearchOutput where
  toJSON (SearchOutput query searchSrc searchImg) =
    object
      [ "query" .= query
      , "searchSrc" .= searchSrc
      , "searchImg" .= (encodeBase64 <$> searchImg)
      ]

instance FromJSON SearchOutput where
  parseJSON = withObject "SearchOutput" $ \v -> do
    query <- v .: "query"
    searchSrc <- v .:? "searchSrc"
    searchImg <- v .:? "searchImg" >>= mapM (either fail return . decodeBase64)
    return $ SearchOutput query searchSrc searchImg

data BrowseOutput = BrowseOutput
  { url          :: String
  , browseSrc    :: Maybe String
  , browseImg    :: Maybe BL.ByteString
  } deriving (Show, Generic)

instance ToJSON BrowseOutput where
  toJSON (BrowseOutput url browseSrc browseImg) =
    object
    [ "url" .= url
    , "browseSrc" .= browseSrc
    , "browseImg" .= (encodeBase64 <$> browseImg)
                            ]
instance FromJSON BrowseOutput where
  parseJSON = withObject "browseOutput" $ \v -> do
    query <- v .: "url"
    searchSrc <- v .:? "browseSrc"
    searchImg <- v .:? "browseImg" >>= mapM (either fail return . decodeBase64)
    return $ BrowseOutput query searchSrc searchImg

data ToolOutput = ToolOutput
  { searchOut :: Maybe SearchOutput
  , browseOut :: Maybe BrowseOutput
  } deriving (Show, Generic)

instance ToJSON ToolOutput
instance FromJSON ToolOutput

data Command = Command 
  { action :: String
  , arg    :: String
  } deriving (Show, Generic)

instance ToJSON Command
instance FromJSON Command

encodeBase64 :: BL.ByteString -> T.Text
encodeBase64 = TE.decodeUtf8 . Base64.encode . BL.toStrict

decodeBase64 :: T.Text -> Either String BL.ByteString
decodeBase64 = fmap BL.fromStrict . Base64.decode . TE.encodeUtf8

search :: String -> IO SearchOutput
search query = do
  sessionIdM <- createSession
  case sessionIdM of
    Nothing -> error "Unable to create browser session" 
    Just sessionId -> do
      goToURL sessionId "https://www.duckduckgo.com"
      elementIdM <- findElement sessionId (T.pack "xpath") (T.pack "//input[@name='q']")
      case elementIdM of
        Nothing -> error "Search bar not found"
        Just elementId -> do
          sendKeys sessionId elementId (T.pack query)
          threadDelay 5000000 -- TODO: Find a better way to wait for search results
          pageSrc <- getPageSource sessionId
          let cleanedPageSrc = cleanHtml (T.unpack <$> pageSrc)
          -- screenshot <- getScreenshot sessionId
          deleteSession sessionId
          return $ SearchOutput query cleanedPageSrc Nothing -- Screenshots come later

browse :: String -> IO BrowseOutput
browse url = do
  session_id <- createSession
  case session_id of 
    Just sid -> do
      goToURL sid (T.pack url)
      pageSrc <- getPageSource sid
      -- screenshot <- getScreenshot sid
      deleteSession sid
      let cleanedPageSrc = cleanHtml (T.unpack <$> pageSrc)
      return $ BrowseOutput url cleanedPageSrc Nothing
    Nothing -> error "Unable to start browser session"

runCommand :: Command -> IO (Maybe ToolOutput)
runCommand cmd = 
  case action cmd of
    "search" -> do
      searchRes <- search (arg cmd)
      return $ Just $ ToolOutput (Just searchRes) Nothing
    "browse" -> do
      browseRes <- browse (arg cmd)
      return $ Just $ ToolOutput Nothing (Just browseRes)
    "finish" -> return Nothing
    _        -> return $ Just $ ToolOutput Nothing Nothing

executeToolUse :: [Command] -> IO [Maybe ToolOutput]
executeToolUse cmds = mapM runCommand cmds

-- RL related data types and functions

data BrowserAction = BrowserAction
  { actionType  :: Int    -- 0: NO_OP, 1: KEYBOARD, 2: MOUSE
  , keyboardKey :: String -- Character to be sent
  , mouseX      :: Int    -- X coordinate
  , mouseY      :: Int    -- Y coordinate
  } deriving (Show, Generic)

instance FromJSON BrowserAction where
  parseJSON = genericParseJSON defaultOptions
      { fieldLabelModifier = camelTo2 '_' 
      }

instance ToJSON BrowserAction where
  toJSON = genericToJSON defaultOptions
      { fieldLabelModifier = camelTo2 '_'
      }

parseSpecial :: T.Text -> T.Text
parseSpecial a = 
  case a of
    "ENTER" -> T.pack "\xE006"
    "SPACE" -> T.pack "\xE00D"
    "BACKSPACE" -> T.pack "\xE003"
    "TAB" -> T.pack "\xE004"
    "ESCAPE" -> T.pack "\xE00C"
    "LEFT" -> T.pack "\xE012"
    "RIGHT" -> T.pack "\xE014"
    "UP" -> T.pack "\xE013"
    "DOWN" -> T.pack "\xE015"
    _ -> a

data RLStatus = RLStatus
  { reward :: Int
  , done   :: Bool
  } deriving (Show, Generic)

instance FromJSON RLStatus
instance ToJSON RLStatus

data EnvStatus = EnvStatus 
  { rlUrl     :: T.Text
  , scrollPos :: Double
  } deriving (Show, Generic)

getBrowserState :: T.Text -> IO EnvStatus
getBrowserState sessionid = do
  maybeState <- executeScript sessionid $
    "return JSON.stringify({ currentUrl: window.location.href, scrollY: window.scrollY });"
  let stateText = fromMaybe (error "Failed to fetch browser state") maybeState
  case eitherDecode (BL.fromStrict $ TE.encodeUtf8 stateText) of
    Right envStatus -> return envStatus
    Left err        -> error $ "Failed to parse browser state: " ++ err

instance FromJSON EnvStatus where
  parseJSON = withObject "EnvStatus" $ \o -> do
    url <- o .: "currentUrl"
    scrollY <- o .: "scrollY"
    return $ EnvStatus url scrollY

calculateReward :: EnvStatus -> EnvStatus -> RLStatus
calculateReward b a = 
  if rlUrl a == "https://en.wikipedia.org/wiki/Reinforcement_learning" then
    RLStatus 100 True
  else if rlUrl b /= rlUrl a then
    RLStatus 10 False
  else if scrollPos a > scrollPos b then
    RLStatus 1 False
  else RLStatus (-1) False

type CachedEnvStatus = TVar (Maybe EnvStatus)

executeAction :: T.Text -> BrowserAction -> CachedEnvStatus -> IO RLStatus
executeAction sessionid action cache = do
  beforeState <- atomically $ readTVar cache
  curBeforeState <- case beforeState of
    Just state -> return state
    Nothing    -> getBrowserState sessionid

  case actionType action of
    0 -> return ()
    1 -> sendKeyboardAction sessionid (parseSpecial (T.pack (keyboardKey action)))
    2 -> sendMouseAction sessionid (mouseX action) (mouseY action)
    _ -> error "Invalid action types"

  afterState <- getBrowserState sessionid
  atomically $ writeTVar cache (Just afterState)
  return $ calculateReward curBeforeState afterState

