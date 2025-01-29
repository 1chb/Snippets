module Main where

import Api (api, server)
import MyLib qualified (someFunc)
import Network.Wai.Handler.Warp (run)
import Servant (serve)
import Database (withDbConnection, queryGreetings)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
  greetings <- withDbConnection queryGreetings
  print greetings
  -- Run the server
  putStrLn "Starting server on port 8080..."
  run 8080 (serve api server)
