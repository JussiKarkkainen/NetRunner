{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module AIClient.AIClient
  ( sendToModel
  , Input(..)
  , Output(..)
  ) where

import Data.Aeson (encode, decode, FromJSON, ToJSON)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Network.HTTP.Client
import Data.ByteString.Lazy.Char8 as L8

data Input = Input 
  { iteration :: Int
  , ogUserPrompt :: String
  , taskCreationTime :: UTCTime
  , curTime :: UTCTime
  , prevInput :: Maybe String
  } deriving (Show, Generic)

instance FromJSON Input
instance ToJSON Input

data ServerOutput = ServerOutput
  { input :: Input
  , outText :: String
  } deriving (Show, Generic)

instance FromJSON Output
instance ToJSON Output

data Output = Output
  { input :: Input
  , outText :: String
  , commands :: [Command]
  } deriving (Show, Generic)

data Command = Command 
  { action :: String
  , arg    :: String
  } deriving (Show, Generic)

extractJsonBlocks :: String -> [String]
extractJsonBlocks [] = []
extractJsonBlocks str = case dropWhile (/= '{') str of
  "" -> []
  jsonStart -> let (json, rest) = span (/= '}') jsonStart
               in (json ++ "}") : extractJsonBlocks (drop 1 rest)

parseJsonBlocks :: [String] -> [Command]
parseJsonBlocks blocks = 
  let decodeResult = map decode blocks
  in catMaybes decodeResults

extractCommands :: String -> [Command]
extractCommands inStr = 
  let jsonBlocks = extractJsonBlocks inStr
  in parseJsonBlocks jsonBlocks

sendToModel :: Input -> IO (Maybe Output)
sendToModel payload = do
  manager <- newManager defaultManagerSettings
  let url = "http://127.0.0.1:5050/model"
  initialRequest <- parseRequest url
  let jsonPayload = encode payload
  let request = initialRequest { method = "POST", requestBody = RequestBodyLBS jsonPayload } 
  response <- httpLbs request manager
  let decodedResponse = decode (responseBody response) :: Maybe ServerOutput
  case decodedResponse of
    Just r -> do
      let commands = extractCommands $ outText decodedResponse
      let outParsed = Output (input decodedResponse) (outText decodedResponse) commands
      return outParsed
    Nothing -> return Nothing

