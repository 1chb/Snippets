module Middleware.Delayer (delayBadRequests) where

import Control.Concurrent (myThreadId, threadDelay)
import Control.Monad qualified as Control
import Network.HTTP.Types (status400, status401, status403, status404, status405, status429)
import Network.Wai (Middleware, responseStatus)
import System.IO (hFlush, stdout)
import System.Random (randomRIO)

delayBadRequests :: Middleware
delayBadRequests app request respond = app request $ \response -> do
  Control.when (responseStatus response `elem` [status400, status401, status403, status404, status405, status429]) do
    thread <- myThreadId
    putStrLn $ show thread ++ ": Delay bad request..."
    hFlush stdout
    delay <- randomRIO (500_000, 2_000_000) -- in microseconds
    threadDelay delay
    putStrLn $ show thread ++ ": ...for " ++ show delay ++ " us."
    hFlush stdout
  respond response
