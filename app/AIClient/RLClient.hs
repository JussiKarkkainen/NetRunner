module AIClient.RLClient
  ( sendThroughSocket
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.WebSockets

sendThroughSocket :: BL.ByteString -> IO ()
sendThroughSocket screenshot = do
  runClient "localhost" 8765 "/" $ \conn -> do
    sendBinData conn screenshot
    putStrLn "Screenshot sent to Python server."

sendBinData :: Connection -> BL.ByteString -> IO ()
sendBinData conn imgData = do
  sendBinaryData conn imgData
  putStrLn "Data sent."
  return ()
