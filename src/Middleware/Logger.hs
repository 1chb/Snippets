module Middleware.Logger (setup) where

import Control.Monad qualified as Control
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Network.Wai (Middleware, Request (remoteHost, requestHeaders))
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Console.ANSI (Color (..), ColorIntensity (Vivid), ConsoleLayer (..), SGR (SetColor, SetDefaultColor), setSGRCode)

setup :: IO Middleware
setup = pure $ ipLogger . logStdoutDev

ipLogger :: Middleware
ipLogger app request respond = do
  let ipAddress = show $ remoteHost request
  currentTime <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
  putStrLn $ "[" ++ foreground Blue timestamp ++ "] IP: " ++ ipAddress
  Control.forM_ (requestHeaders request) $ putStrLn . ("  " ++) . show
  app request respond

foreground :: Color -> String -> String
foreground color text =
  setSGRCode [SetColor Foreground Vivid color] ++ text ++ setSGRCode [SetDefaultColor Foreground]
