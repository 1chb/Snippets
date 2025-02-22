module Main where

import Api (api, server)
import Config.Load qualified as Config
import Data.Function ((&))
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Database qualified
import Middleware.Delayer (delayBadRequests)
import Middleware.Logger qualified as Logger
import Network.Wai.Handler.Warp qualified as Wai (defaultSettings, run, setOnException, setPort)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Options (Options (development, port), getOptions)
import Servant qualified (layoutWithContext, serveWithContext)
import Session qualified

main :: IO ()
main = do
  opts <- getOptions
  logger <- Logger.setup
  sessionEnv <- Config.get
  TIO.putStr $ Servant.layoutWithContext api (Session.authContext sessionEnv)
  TIO.putStrLn $ "Starting server at port " <> T.show (port opts)
  let runner =
        if development opts
          then \app -> do
            print sessionEnv -- show this in development mode only.
            Wai.run (port opts) app
          else
            let certFile = "/etc/letsencrypt/live/wv2.hopto.org/fullchain.pem"
                keyFile = "/etc/letsencrypt/live/wv2.hopto.org/privkey.pem"
             in runTLS (tlsSettings certFile keyFile) $
                  Wai.defaultSettings
                    & Wai.setOnException Logger.requestException
                    & Wai.setPort (port opts)
  dbEnv <- Database.connectAndMigrate
  runner
    . delayBadRequests
    . logger
    . Servant.serveWithContext api (Session.authContext sessionEnv)
    $ server dbEnv sessionEnv
