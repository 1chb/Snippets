module Main where

import Api (api, server)
import Database qualified (connectAndMigrate)
import MyLib qualified (someFunc)
import Network.Wai.Handler.Warp qualified as Wai (run)
import Servant qualified (serve)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
  -- Run the server
  putStrLn "Starting server on port 8080..."
  Database.connectAndMigrate >>= Wai.run 8080 . Servant.serve api . server
