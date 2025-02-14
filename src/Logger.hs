module Logger (setup) where

import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.IO (hFlush, stdout)

setup :: IO Middleware
setup = do
  pure $ timestampLogger . logStdoutDev

timestampLogger :: Middleware
timestampLogger app request sendResponse = do
  currentTime <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
  putStrLn $ "[" ++ timestamp ++ "] Request received"
  app request $ \response -> do
    putStrLn $ "[" ++ timestamp ++ "] Response sent"
    hFlush stdout
    sendResponse response
