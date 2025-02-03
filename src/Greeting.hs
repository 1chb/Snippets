{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Greeting (Form, table, form, get, add) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Text qualified as T
import Database qualified as DB
import GHC.Generics (Generic)
import Lucid (Html, action_, body_, form_, h1_, head_, html_, input_, method_, name_, placeholder_, table_, td_, title_, toHtml, tr_, type_, value_)
import Web.FormUrlEncoded (FromForm (..), parseUnique)

data Form where
  Form :: {greeting :: Text} -> Form
  deriving stock (Generic)

instance FromForm Form where
  fromForm f = Form <$> parseUnique "greeting" f

table :: [String] -> Html ()
table gs = table_ $ mapM_ (tr_ . td_ . toHtml) gs

form :: Html ()
form = form_ [method_ "post", action_ "/add-greeting"] $ do
  input_ [type_ "text", name_ "greeting", placeholder_ "Enter new greeting"]
  input_ [type_ "submit", value_ "Add Greeting"]

get :: (MonadIO m) => DB.Environment -> m (Html ())
get env = do
  greetings <- liftIO $ DB.queryGreetings env
  return $ html_ $ do
    head_ $ title_ "Greetings Table"
    body_ $ do
      h1_ "List of Greetings"
      table greetings
      form

add :: (MonadIO m) => DB.Environment -> Form -> m (Html ())
add env (Form newG) = do
  liftIO $ DB.insertGreeting env $ T.unpack newG
  get env
