module Main where

import Api (api, server)
import Database qualified (connectAndMigrate)
import Network.Wai.Handler.Warp qualified as Wai (defaultSettings, run, setPort)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Servant qualified (serve)
import Options (Options(local, port), getOptions)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  opts <- getOptions
  print opts  -- For demonstration purposes, print the parsed options
  let (runner, port') =
        if local opts
        then (Wai.run $ port opts, port opts)
        else do
          let tlsConfig = tlsSettings "/etc/letsencrypt/live/wv2.hopto.org/fullchain.pem" "/etc/letsencrypt/live/wv2.hopto.org/privkey.pem"
          (runTLS tlsConfig $ Wai.setPort 443 Wai.defaultSettings, 443)
  putStrLn $ "Starting server: " ++ show port'
  Database.connectAndMigrate
    >>= runner
      . Servant.serve api
      . server
