{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module AIClient.AIClient
  ( sendToModel
  , Input(..)
  , Output(..)
  , Command(..)
  ) where

import Data.Aeson (encode, decode, FromJSON, ToJSON)
import Data.Time (UTCTime)
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Network.HTTP.Client
import Data.ByteString.Lazy.Char8 as L8 hiding (map, span, dropWhile, drop)

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

instance FromJSON ServerOutput
instance ToJSON ServerOutput

data Output = Output
  { formatInput :: Input
  , formatOutText :: String
  , formatCommands :: [Command]
  } deriving (Show, Generic)

instance FromJSON Output
instance ToJSON Output

data Command = Command 
  { action :: String
  , arg    :: String
  } deriving (Show, Generic)

instance FromJSON Command
instance ToJSON Command

extractJsonBlocks :: String -> [String]
extractJsonBlocks [] = []
extractJsonBlocks str = case dropWhile (/= '{') str of
  "" -> []
  jsonStart -> let (json, rest) = span (/= '}') jsonStart
               in (json ++ "}") : extractJsonBlocks (drop 1 rest)

parseJsonBlocks :: [String] -> [Command]
parseJsonBlocks blocks = 
  let decodeResult = map (decode . L8.pack) blocks
  in catMaybes decodeResult

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
      let commands = extractCommands $ outText r
      let outParsed = Output (input r) (outText r) commands
      return $ Just outParsed
    Nothing -> return Nothing

