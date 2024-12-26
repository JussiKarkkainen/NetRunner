{-# LANGUAGE OverloadedStrings #-}

module ToolUse.GeckoDriver 
  ( createSession
  , goToURL
  , resizeViewport
  , getPageSource
  , getCurrentURL
  , executeScript
  , getScreenshot
  , deleteSession
  , sendKeys
  , findElement
  , sendMouseAction
  , sendKeyboardAction
  ) where

import Network.HTTP.Client
import Data.Aeson
import Data.Aeson.Types (parseEither, parseMaybe)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString as BS
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
  print response
  return $ responseBody response

sendGetRequest :: T.Text -> IO BL.ByteString
sendGetRequest url = do
  manager <- newManager defaultManagerSettings
  request <- parseRequest (T.unpack url)
  response <- httpLbs request manager
  return $ responseBody response

resizeViewport :: T.Text -> Int -> Int -> IO ()
resizeViewport sessionId h w = do
  let resizeBody = encode $ object
          [ "width" .= w
          , "height" .= h
          ]
  let resizeUrl = "http://localhost:4444/session/" <> sessionId <> "/window/rect"
  response <- sendPostRequest (T.unpack resizeUrl) resizeBody
  return ()

getCurrentURL :: T.Text -> IO (Maybe T.Text)
getCurrentURL sessionId = do
  let url = "htto://localhost:4444/session/" <> sessionId <> "url"
  response <- sendGetRequest url
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

executeScript :: T.Text -> String -> IO (Maybe T.Text)
executeScript sessionId script = do
  let url = "http://localhost:4444/session/" <> sessionId <> "/execute/sync"
  let requestBody = encode $ object
          [ "script" .= script
          , "args" .= ([] :: [Value])
          ]
  response <- sendPostRequest (T.unpack url) requestBody
  case decode response of
    Just (Object obj) -> do
      case parseEither (.: "value") obj of
        Left err -> do
          putStrLn $ "Error extracting 'value' from JSON: " ++ err
          return Nothing
        Right value -> return (Just value)
      -- return $ (Just $ TE.decodeUtf8 (BL.toStrict value))
    _ -> return $ Nothing 
      
goToURL :: T.Text -> T.Text -> IO ()
goToURL sessionId url = do
  let endpoint = "http://localhost:4444/session/" ++ T.unpack sessionId ++ "/url"
      body = encode $ object ["url" .= url]
  response <- sendPostRequest endpoint body
  return ()

sendMouseAction :: T.Text -> Int -> Int -> IO ()
sendMouseAction sessionId x y = do
  let url = T.unpack $ T.concat ["http://localhost:4444/session/", sessionId, "/actions"]
  let body = encode $ object
        [ "actions" .= 
          [ object
            [ "type" .= ("pointer" :: T.Text)
            , "id" .= ("pointer1" :: T.Text)
            , "actions" .=
              [ object
                [ "type" .= ("pointerMove" :: T.Text)
                , "duration" .= (0 :: Int)
                , "x" .= x
                , "y" .= y
                ]
              , object
                [ "type" .= ("pointerDown" :: T.Text)
                , "button" .= (0 :: Int) -- Left mouse button
                ]
              , object
                [ "type" .= ("pointerUp" :: T.Text)
                , "button" .= (0 :: Int) -- Left mouse button
                ]
              ]
            ]
          ]
        ]
  _ <- sendPostRequest url body
  return ()

sendKeyboardAction :: T.Text -> T.Text -> IO ()
sendKeyboardAction sessionId text = do
  let url = T.unpack $ T.concat ["http://localhost:4444/session/", sessionId, "/actions"]
  let actions = map (\char -> object
        [ "type" .= ("keyDown" :: T.Text)
        , "value" .= T.singleton char
        ]) (T.unpack text)
        ++ map (\char -> object
        [ "type" .= ("keyUp" :: T.Text)
        , "value" .= T.singleton char
        ]) (T.unpack text)

  let body = encode $ object
        [ "actions" .= 
          [ object
            [ "type" .= ("key" :: T.Text)
            , "id" .= ("default" :: T.Text)
            , "actions" .= actions
            ]
          ]
        ]
  _ <- sendPostRequest url body
  return ()

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
          case decoded of
            Left err -> do
              putStrLn $ "Base64 decode error: " <> (show err)
              return Nothing
            Right bytes -> do
              return $ Just (BL.fromStrict bytes)
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

