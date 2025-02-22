module Middleware.Logger (setup, requestException) where

import Control.Concurrent (myThreadId)
import Control.Exception (SomeException)
import Control.Monad qualified as Control
import Data.ByteString.Char8 qualified as BS
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Network.Socket (SockAddr (..))
import Network.Wai (Middleware, Request (remoteHost, requestHeaders))
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Console.ANSI (Color (..), ColorIntensity (Vivid), ConsoleLayer (..), SGR (SetColor, SetDefaultColor), setSGRCode)
import System.IO (hFlush, stdout)

setup :: IO Middleware
setup = pure $ ipLogger . logStdoutDev

ipLogger :: Middleware
ipLogger app request respond = do
  let ipAddress = T.show $ remoteHost request
  currentTime <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
  thread <- myThreadId
  TIO.putStrLn $ "[" <> T.pack (foreground Blue timestamp) <> "] - " <> T.show thread <> " - " <> ipAddress
  Control.forM_ (requestHeaders request) $ TIO.putStrLn . ("  " <>) . T.show
  hFlush stdout
  app request respond

foreground :: Color -> String -> String
foreground color text =
  setSGRCode [SetColor Foreground Vivid color] ++ text ++ setSGRCode [SetDefaultColor Foreground]

requestException :: Maybe Request -> SomeException -> IO ()
requestException mReq exception = do
  currentTime <- getCurrentTime
  thread <- myThreadId
  let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
      clientIP = maybe "<no request>" (showSockAddr . remoteHost) mReq
  BS.putStrLn $
    ("[" <> BS.pack (foreground Red timestamp))
      <> ("] - " <> BS.pack (show thread))
      <> (" - " <> clientIP)
      <> (" - " <> BS.pack (show exception))
  hFlush stdout

showSockAddr :: SockAddr -> BS.ByteString
showSockAddr = \case
  SockAddrInet _ hostAddr -> BS.pack $ show hostAddr
  SockAddrInet6 _ _ hostAddr6 _ -> BS.pack $ show hostAddr6
  SockAddrUnix path -> BS.pack path
