module Main where

import Api (api, server)
import Database qualified (connectAndMigrate)
import Login.Session qualified (generateEnv)
import Network.Wai.Handler.Warp qualified as Wai (defaultSettings, run, setPort)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Options (Options (local, port), getOptions)
import Servant qualified (serve)

main :: IO ()
main = do
  opts <- getOptions
  putStrLn $ "Starting server at port " ++ show (port opts)
  let runner =
        if local opts
          then Wai.run $ port opts
          else
            let tlsConfig = tlsSettings "/etc/letsencrypt/live/wv2.hopto.org/fullchain.pem" "/etc/letsencrypt/live/wv2.hopto.org/privkey.pem"
             in runTLS tlsConfig $ Wai.setPort (port opts) Wai.defaultSettings
  dbEnv <- Database.connectAndMigrate
  sessionEnv <- Login.Session.generateEnv
  print sessionEnv
  runner
    . Servant.serve api
    $ server dbEnv sessionEnv
