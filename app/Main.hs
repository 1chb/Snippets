module Main where

import Api (api, server)
import Database qualified (connectAndMigrate)
import MyLib qualified (someFunc)
import Network.Wai.Handler.Warp qualified as Wai (defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Servant qualified (serve)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
  -- Run the server
  let port = 443
  putStrLn $ "Starting server: " ++ show port
  let tlsConfig = tlsSettings "/etc/letsencrypt/live/wv2.hopto.org/fullchain.pem" "/etc/letsencrypt/live/wv2.hopto.org/privkey.pem"
  Database.connectAndMigrate
    >>= runTLS tlsConfig (Wai.setPort port Wai.defaultSettings)
      . Servant.serve api
      . server
