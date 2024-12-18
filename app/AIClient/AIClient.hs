{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module AIClient.AIClient
  ( sendToModel
  , Input(..)
  , ServerOutput(..)
  , IterationOutput(..)
  ) where

import Data.Aeson (encode, decode, FromJSON, ToJSON)
import Data.Time (UTCTime)
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Network.HTTP.Client
import Data.ByteString.Lazy.Char8 as L8 hiding (map, span, dropWhile, drop)
import ToolUse.ToolUse (ToolOutput(..), Command(..))

data Input = Input 
  { iteration :: Int
  , ogUserPrompt :: String
  , taskCreationTime :: UTCTime
  , curTime :: UTCTime
  , prevOutput :: [IterationOutput]
  } deriving (Show, Generic)

instance ToJSON Input
instance FromJSON Input

data ServerOutput = ServerOutput
  { formatOutText :: String
  , formatCommands :: [Command]
  } deriving (Show, Generic)

instance ToJSON ServerOutput
instance FromJSON ServerOutput

data IterationOutput = IterationOutput
  { serverOutput    :: ServerOutput
  , toolResults  :: [ToolOutput]
  } deriving (Show, Generic)

instance ToJSON IterationOutput
instance FromJSON IterationOutput

data ServerResponse = ServerResponse 
  { output :: String
  } deriving (Show, Generic)

instance ToJSON ServerResponse
instance FromJSON ServerResponse

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

sendToModel :: Input -> IO (Maybe ServerOutput)
sendToModel payload = do
  manager <- newManager defaultManagerSettings
  let url = "http://127.0.0.1:5050/model"
  initialRequest <- parseRequest url
  let jsonPayload = encode payload
  let request = initialRequest { method = "POST", requestBody = RequestBodyLBS jsonPayload } 
  response <- httpLbs request manager
  let decodedResponse = decode (responseBody response) :: Maybe ServerResponse
  case decodedResponse of
    Just r -> do
      let resString = output r
      let commands = extractCommands resString
      let outParsed = ServerOutput resString commands
      return $ Just outParsed
    Nothing -> return Nothing

