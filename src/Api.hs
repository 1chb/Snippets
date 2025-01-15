{-# LANGUAGE DataKinds #-}

module Api (api, server) where

import Servant

-- Define the API type
type API = "hello" :> Get '[PlainText] String

-- Implement the server
server :: Server API
server = return "Hello, world!"

-- Create a Proxy for the API
api :: Proxy API
api = Proxy
