module Main where

import Api (api, server)
import Config.Load qualified as Config
import Database qualified
import Logger qualified
import Network.Wai.Handler.Warp qualified as Wai (defaultSettings, run, setPort)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Options (Options (development, port), getOptions)
import Servant qualified (serveWithContext)
import Session qualified

main :: IO ()
main = do
  opts <- getOptions
  logger <- Logger.setup
  putStrLn $ "Starting server at port " ++ show (port opts)
  sessionEnv <- Config.get
  let runner =
        if development opts
          then \app -> do
            print sessionEnv -- show this in development mode only.
            Wai.run (port opts) app
          else
            let tlsConfig = tlsSettings "/etc/letsencrypt/live/wv2.hopto.org/fullchain.pem" "/etc/letsencrypt/live/wv2.hopto.org/privkey.pem"
             in runTLS tlsConfig $ Wai.setPort (port opts) Wai.defaultSettings
  dbEnv <- Database.connectAndMigrate
  runner
    . logger
    . Servant.serveWithContext api (Session.authContext sessionEnv)
    $ server dbEnv sessionEnv
