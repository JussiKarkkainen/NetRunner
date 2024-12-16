{-# Language OverloadedStrings #-}

module ToolUse.GeckoDriver 
  ( createSession
  , goToURL
  ) where

import Network.HTTP.Client
import Data.Aeson
import Data.ByteString.Base64 (decode)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T


data SessionResponse = SessionResponse
  { sessionId :: T.Text
  } deriving (Show)

instance FromJSON SessionResponse where
  parseJSON = withObject "SessionResponse" $ \v -> do
    value <- v .: "value"
    sessionId <- value .: "sessionId"
    return $ SessionResponse sessionId 

createSession :: IO (Maybe T.Text)
createSession = do
  let body = encode $ object 
        [ "capabilities" .= object
          [ "alwaysMatch" .= object
            [ "browserName" .= ("firefox" :: T.Text)
            ]
          ]
        ] 

  response <- sendPostRequest "http://localhost:4444/session" body
  let session = decode response :: Maybe SessionResponse
  print session
  return (sessionId <$> session)

sendPostRequest :: String -> BL.ByteString -> IO BL.ByteString
sendPostRequest url body = do
  manager <- newManager defaultManagerSettings
  initialRequest <- parseRequest url
  let request = initialRequest
        { method = "POST"
        , requestBody = RequestBodyLBS body
        , requestHeaders =
            [ ("Content-Type", "application/json")
            ]
        }
  response <- httpLbs request manager
  return $ responseBody response

goToURL :: String -> String -> IO ()
goToURL sessionId url = do
  let endpoint = "http://localhost:4444/session/" ++ sessionId ++ "/url"
  let body = encode $ object ["url" .= url]
  response <- sendPostRequest endpoint body
  BL.putStr response

getPageSource :: T.Text -> IO (Maybe Text)
getPageSource sessionId = do
  let url = "http://localhost:4444/session/" <> sessionId <> "/source"
  response <- sendGetRequest url
  let html = decode response :: Maybe (Object "value" Text)
  return html

getScreenshot :: T.Text -> IO (Maybe BL.ByteString)
getScreenshot sessionId = do
  let url = "http://localhost:4444/session/" <> sessionId <> "/screenshot"
  response <- sendGetRequest url
  case decode <$> decode response :: Maybe Text of
    Just (Right screenshot) -> return (Just screenshot)
    _ -> return Nothing

