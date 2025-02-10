module Main where

import Api (api, server)
import Database qualified
import Login.Session qualified as Session
import Network.Wai.Handler.Warp qualified as Wai (defaultSettings, run, setPort)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Options (Options (local, port), getOptions)
import Servant qualified (serveWithContext)

main :: IO ()
main = do
  opts <- getOptions
  putStrLn $ "Starting server at port " ++ show (port opts)
  sessionEnv <- Session.generateEnv
  let runner =
        if local opts
          then \app -> do
            print sessionEnv -- show this in local mode only.
            Wai.run (port opts) app
          else
            let tlsConfig = tlsSettings "/etc/letsencrypt/live/wv2.hopto.org/fullchain.pem" "/etc/letsencrypt/live/wv2.hopto.org/privkey.pem"
             in runTLS tlsConfig $ Wai.setPort (port opts) Wai.defaultSettings
  dbEnv <- Database.connectAndMigrate
  runner
    . Servant.serveWithContext api (Session.authContext sessionEnv)
    $ server dbEnv sessionEnv
