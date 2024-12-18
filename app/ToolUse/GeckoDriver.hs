{-# LANGUAGE OverloadedStrings #-}

module ToolUse.GeckoDriver 
  ( createSession
  , goToURL
  , getPageSource
  , getScreenshot
  , deleteSession
  , sendKeys
  , findElement
  ) where

import Network.HTTP.Client
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base64 as Base64
import Data.Aeson.Lens (key, _String)
import Control.Lens ((^?))

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
            , "moz:firefoxOptions" .= object
              [ "args" .= [ "-headless" :: T.Text ]
              ]
            ]
          ]
        ]

  response <- sendPostRequest "http://localhost:4444/session" body
  let session = decode response :: Maybe SessionResponse
  print session
  return (sessionId <$> session)

deleteSession :: T.Text -> IO ()
deleteSession sessionId = do
  let url = "http://localhost:4444/session/" <> T.unpack sessionId
  _ <- sendDeleteRequest url
  return ()

sendDeleteRequest :: String -> IO BL.ByteString
sendDeleteRequest url = do
  manager <- newManager defaultManagerSettings
  initialRequest <- parseRequest url
  let request = initialRequest { method = "DELETE" }
  response <- httpLbs request manager
  return $ responseBody response

sendPostRequest :: String -> BL.ByteString -> IO BL.ByteString
sendPostRequest url body = do
  manager <- newManager defaultManagerSettings
  initialRequest <- parseRequest url
  let request = initialRequest
        { method = "POST"
        , requestBody = RequestBodyLBS body
        , requestHeaders =
            [ ("Content-Type", "application/json") ]
        }
  response <- httpLbs request manager
  return $ responseBody response

goToURL :: T.Text -> T.Text -> IO ()
goToURL sessionId url = do
  let endpoint = "http://localhost:4444/session/" ++ T.unpack sessionId ++ "/url"
      body = encode $ object ["url" .= url]
  response <- sendPostRequest endpoint body
  BL.putStr response

sendGetRequest :: T.Text -> IO BL.ByteString
sendGetRequest url = do
  manager <- newManager defaultManagerSettings
  request <- parseRequest (T.unpack url)
  response <- httpLbs request manager
  return $ responseBody response

getPageSource :: T.Text -> IO (Maybe T.Text)
getPageSource sessionId = do
  let url = "http://localhost:4444/session/" ++ T.unpack sessionId ++ "/source"
  response <- sendGetRequest (T.pack url)
  case decode response :: Maybe Value of
    Just (Object obj) -> 
      case parseEither (.: "value") obj of
        Left err -> do
          putStrLn $ "Error extracting 'value' from JSON: " ++ err
          return Nothing
        Right value -> return (Just value)
    _ -> do
      putStrLn "Failed to decode JSON response."
      return Nothing

getScreenshot :: T.Text -> IO (Maybe BL.ByteString)
getScreenshot sessionId = do
  let url = "http://localhost:4444/session/" <> sessionId <> "/screenshot"
  response <- sendGetRequest url
  case decode response :: Maybe Value of
    Just (Object obj) -> do
      case parseEither (.: "value") obj of
        Left err -> do
          putStrLn $ "Error extracting 'value' from JSON: " ++ err
          return Nothing
        Right base64Str -> do
          let decoded = Base64.decode (TE.encodeUtf8 base64Str)
          return $ either (const Nothing) (Just . BL.fromStrict) decoded
    _ -> do
      putStrLn "Failed to decode JSON response."
      return Nothing

sendKeys :: T.Text -> T.Text -> T.Text -> IO ()
sendKeys sessionId elementId keys = do
  let endpoint = "http://localhost:4444/session/" ++ T.unpack sessionId ++ "/element/" ++ T.unpack elementId ++ "/value"
      body = encode $ object ["text" .= keys, "value" .= [keys]]
  _ <- sendPostRequest endpoint body

  let enterBody = encode $ object ["text" .= ("\xE007" :: T.Text), "value" .= ["\xE007" :: T.Text]]
  _ <- sendPostRequest endpoint enterBody
  return ()

findElement :: T.Text -> T.Text -> T.Text -> IO (Maybe T.Text)
findElement sessionId strategy selector = do
  let endpoint = "http://localhost:4444/session/" ++ T.unpack sessionId ++ "/element"
      body = encode $ object ["using" .= strategy, "value" .= selector]
  response <- sendPostRequest endpoint body
  case decode response :: Maybe Value of
    Just obj -> return $ obj ^? key "value" . key "element-6066-11e4-a52e-4f735466cecf" . _String
    Nothing  -> return Nothing

