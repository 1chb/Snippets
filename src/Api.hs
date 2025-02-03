{-# LANGUAGE DataKinds #-}

module Api (api, server) where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Database qualified as DB
import Greeting qualified
import Lucid (Html)
import Servant (FormUrlEncoded, Get, Header, Headers (..), JSON, PlainText, Post, Proxy (..), ReqBody, Server, addHeader, (:<|>) (..), (:>))
import Servant.HTML.Lucid (HTML)

type API =
  Get '[HTML] (Html ())
    :<|> "add-greeting" :> ReqBody '[FormUrlEncoded] Greeting.Form :> Post '[HTML] (Html ())
    :<|> "old" :> Get '[PlainText] String
    :<|> "hello" :> Hellos
    :<|> "users" :> Get '[JSON] (Headers '[Header "User-Count" Integer] [String])

type Hellos =
  Header "User-Agent" String :> Get '[PlainText] String
    :<|> "there" :> Get '[PlainText] String

server :: DB.Environment -> Server API
server pool =
  Greeting.get pool
    :<|> Greeting.add pool
    :<|> (concat <$> liftIO (DB.queryGreetings pool))
    :<|> fmap ("Hello, " <>) <$> hellos -- Only hello to snd!
    :<|> return (addHeader 2 ["X", "Y"])
  where
    hellos = return . fromMaybe "???" :<|> return "there!"

api :: Proxy API
api = Proxy
