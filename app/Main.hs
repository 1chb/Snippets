module Main where

import Api (api, server)
import MyLib qualified (someFunc)
import Network.Wai.Handler.Warp (run)
import Servant (serve)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
  -- Run the server
  putStrLn "Starting server on port 8080..."
  run 8080 (serve api server)
