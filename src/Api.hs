{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Api (api, server) where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Database qualified as DB
import Greeting qualified
import Lucid (Html)
import Servant (FormUrlEncoded, Get, Header, Headers (..), JSON, NoContent (..), OctetStream, PlainText, Post, Proxy (..), ReqBody, Server, addHeader, err303, errHeaders, throwError, (:<|>) (..), (:>))
import Servant.HTML.Lucid (HTML)

type API =
  "favicon.ico" :> Get '[OctetStream] NoContent
    :<|> "greeting" :> Get '[HTML] (Html ())
    :<|> "add-greeting" :> ReqBody '[FormUrlEncoded] Greeting.Form :> Post '[HTML] NoContent
    :<|> "old" :> Get '[PlainText] String
    :<|> "hello" :> Hellos
    :<|> "users" :> Get '[JSON] (Headers '[Header "User-Count" Integer] [String])

type Hellos =
  Header "User-Agent" String :> Get '[PlainText] String
    :<|> "there" :> Get '[PlainText] String

server :: DB.Environment -> Server API
server env =
  getFavicon
    :<|> Greeting.get env
    :<|> (\form -> Greeting.add env form >> redirectTo "/")
    :<|> (concat <$> liftIO (DB.queryGreetings env))
    :<|> fmap ("Hello, " <>) <$> hellos -- Only hello to snd!
    :<|> return (addHeader 2 ["X", "Y"])
  where
    getFavicon = return NoContent
    hellos = return . fromMaybe "???" :<|> return "there!"
    redirectTo path = throwError err303 {errHeaders = [("Location", path)]}

api :: Proxy API
api = Proxy
