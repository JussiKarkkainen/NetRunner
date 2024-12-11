{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module AIClient.AIClient
  ( sendToModel
  , Input(..)
  , Output(..)
  ) where

import Data.Aeson (encode, FromJSON, ToJSON)
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

data Output = Output
  { input :: Input
  , outText :: String
  } deriving (Show, Generic)

instance FromJSON Output
instance ToJSON Output

sendToModel :: Input -> IO ()
sendToModel payload = do
  manager <- newManager defaultManagerSettings
  let url = "http://127.0.0.1:5050/model"
  initialRequest <- parseRequest url
  let jsonPayload = encode payload
  let request = initialRequest { method = "POST", requestBody = RequestBodyLBS jsonPayload } 
  response <- httpLbs request manager
  L8.putStrLn $ responseBody response

