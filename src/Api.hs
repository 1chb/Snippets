{-# LANGUAGE DataKinds #-}

module Api (api, server) where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Database (queryGreetings)
import Database.Persist.Postgresql (ConnectionPool)
import Servant (Get, Header, Headers (..), JSON, PlainText, Proxy (..), Server, addHeader, (:<|>) (..), (:>))

-- Define the API type
type API =
  Get '[PlainText] String
    :<|> "hello" :> Hellos
    :<|> "users" :> Get '[JSON] (Headers '[Header "User-Count" Integer] [String])

type Hellos =
  Header "User-Agent" String :> Get '[PlainText] String
    :<|> "there" :> Get '[PlainText] String

-- Implement the server
server :: ConnectionPool -> Server API
server pool =
  (concat <$> liftIO (queryGreetings pool))
    :<|> fmap ("Hello, " <>) <$> hellos -- Only hello to snd!
    :<|> return (addHeader 2 ["X", "Y"])
  where
    hellos = return . fromMaybe "???" :<|> return "there!"

-- Create a Proxy for the API
api :: Proxy API
api = Proxy
