{-# LANGUAGE OverloadedRecordDot #-}

module Logger (setup) where

import Control.Monad qualified as Control
import Control.Monad.Logger (fromLogStr, toLogStr)
import Data.ByteString qualified as BS
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger
  ( Callback,
    Destination (Callback, Handle, Logger),
    RequestLoggerSettings (autoFlush),
    defaultRequestLoggerSettings,
    destination,
    logStdoutDev,
    mkRequestLogger,
  )
import System.IO (hFlush, stdout)
import System.Log.FastLogger.LoggerSet (flushLogStr, pushLogStr)

setup :: IO Middleware
setup =
  if True
    then do
      let settings = defaultRequestLoggerSettings
      mkRequestLogger settings {destination = Callback $ timestampCallback settings}
    else do
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

timestampCallback :: RequestLoggerSettings -> Callback
timestampCallback requestLoggerSettings logStr = do
  currentTime <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "[%Y-%m-%d %H:%M:%S] " currentTime
  callback $ toLogStr timestamp <> logStr
  Control.when requestLoggerSettings.autoFlush flush
  where
    (callback, flush) =
      -- This code was copied from mkRequestLogger
      case destination requestLoggerSettings of
        Handle h -> (BS.hPutStr h . fromLogStr, hFlush h)
        Logger l -> (pushLogStr l, flushLogStr l)
        Callback c -> (c, return ())
