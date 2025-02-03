{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Api (api, server) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Database (insertGreeting, queryGreetings)
import Database.Persist.Postgresql (ConnectionPool)
import GHC.Generics (Generic)
import Lucid (Html, action_, body_, form_, h1_, head_, html_, input_, method_, name_, placeholder_, table_, td_, title_, toHtml, tr_, type_, value_)
import Servant (FormUrlEncoded, Get, Header, Headers (..), JSON, PlainText, Post, Proxy (..), ReqBody, Server, addHeader, (:<|>) (..), (:>))
import Servant.HTML.Lucid (HTML)
import Web.FormUrlEncoded 

-- Define the API type
type API =
  Get '[HTML] (Html ())
    :<|> "add-greeting" :> ReqBody '[FormUrlEncoded] GreetingForm :> Post '[HTML] (Html ())
    :<|> "old" :> Get '[PlainText] String
    :<|> "hello" :> Hellos
    :<|> "users" :> Get '[JSON] (Headers '[Header "User-Count" Integer] [String])

type Hellos =
  Header "User-Agent" String :> Get '[PlainText] String
    :<|> "there" :> Get '[PlainText] String

data GreetingForm where
  GreetingForm :: {greeting :: Text} -> GreetingForm
  deriving stock (Generic)

instance FromForm GreetingForm where
  fromForm f = GreetingForm <$> parseUnique "greeting" f


greetingsTable :: [String] -> Html ()
greetingsTable gs = table_ $ mapM_ (tr_ . td_ . toHtml) gs

greetingForm :: Html ()
greetingForm = form_ [method_ "post", action_ "/add-greeting"] $ do
  input_ [type_ "text", name_ "greeting", placeholder_ "Enter new greeting"]
  input_ [type_ "submit", value_ "Add Greeting"]

getGreetings :: (MonadIO m) => ConnectionPool -> m (Html ())
getGreetings pool = do
  greetings <- liftIO (queryGreetings pool)
  return $ html_ $ do
    head_ $ title_ "Greetings Table"
    body_ $ do
      h1_ "List of Greetings"
      greetingsTable greetings
      greetingForm

addGreeting :: (MonadIO m) => ConnectionPool -> GreetingForm -> m (Html ())
addGreeting pool (GreetingForm newG) = do
  liftIO $ insertGreeting pool $ T.unpack newG
  getGreetings pool

-- Implement the server
server :: ConnectionPool -> Server API
server pool =
  getGreetings pool
    :<|> addGreeting pool
    :<|> (concat <$> liftIO (queryGreetings pool))
    :<|> fmap ("Hello, " <>) <$> hellos -- Only hello to snd!
    :<|> return (addHeader 2 ["X", "Y"])
  where
    hellos = return . fromMaybe "???" :<|> return "there!"

-- Create a Proxy for the API
api :: Proxy API
api = Proxy
