{-# Language OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ToolUse.ToolUse 
  ( executeToolUse
  , executeAction
  , Command(..)
  , ToolOutput(..)
  , BrowserEnvActionSpace(..)
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text.Encoding as TE
import Data.Aeson.Types (object, withObject, Parser)
import ToolUse.GeckoDriver 
import ToolUse.HtmlParser (cleanHtml)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), (.:?))
import GHC.Generics (Generic)
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

data MouseAction = MouseAction
  { mActionType :: Int
  , x           :: Double
  , y           :: Double
  } deriving (Show, Generic)

data KeyboardAction = KeyboardAction
  { kActionType :: Int
  , key         :: Int
  , kModifiers   :: [Int]
  } deriving (Show, Generic)

data BrowserEnvActionSpace = BrowserEnvActionSpace
  { mouseAction    :: MouseAction
  , keyboardAction :: KeyboardAction
  } deriving (Show, Generic)

instance FromJSON MouseAction
instance ToJSON MouseAction

instance FromJSON KeyboardAction
instance ToJSON KeyboardAction

instance FromJSON BrowserEnvActionSpace
instance ToJSON BrowserEnvActionSpace


executeAction :: T.Text -> BrowserEnvActionSpace -> IO ()
executeAction sessionid action = print action
