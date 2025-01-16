{-# LANGUAGE DataKinds #-}

module Api (api, server) where

import Data.Maybe (fromMaybe)
import Servant (Get, Header, Headers (..), JSON, PlainText, Proxy (..), Server, addHeader, (:<|>) (..), (:>))

-- Define the API type
type API =
  Get '[PlainText] String
    :<|> "hello"
      :> ( Header "User-Agent" String :> Get '[PlainText] String
             :<|> "there" :> Get '[PlainText] String
         )
    :<|> "users" :> Get '[JSON] (Headers '[Header "User-Count" Integer] [String])

-- Implement the server
server :: Server API
server =
  return "tack"
    :<|> fmap ("Hello, " <>)
      <$> ( (return . fromMaybe "???")
              :<|> return "there!"
          )
    :<|> return (addHeader 2 ["X", "Y"])

-- Create a Proxy for the API
api :: Proxy API
api = Proxy
