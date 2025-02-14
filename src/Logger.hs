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
    mkRequestLogger,
  )
import System.IO (hFlush)
import System.Log.FastLogger.LoggerSet (flushLogStr, pushLogStr)

setup :: IO Middleware
setup = do
  let settings = defaultRequestLoggerSettings
  mkRequestLogger settings {destination = Callback $ timestampCallback settings}

timestampCallback :: RequestLoggerSettings -> Callback
timestampCallback settings logStr = do
  currentTime <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "[%Y-%m-%d %H:%M:%S] " currentTime
  callback $ toLogStr timestamp <> logStr
  Control.when settings.autoFlush flush
  where
    (callback, flush) =
      -- This code was copied from mkRequestLogger
      case destination settings of
        Handle h -> (BS.hPutStr h . fromLogStr, hFlush h)
        Logger l -> (pushLogStr l, flushLogStr l)
        Callback c -> (c, return ())
