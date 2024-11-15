module Main where

import WebServer.WebServer (runServer)

main :: IO ()
main = do 
  runServer 8080

