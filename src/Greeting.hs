{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Greeting (Form, get, add) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Text qualified as T
import Database qualified as DB
import GHC.Generics (Generic)
import Lucid (Html, action_, body_, button_, disabled_, form_, h1_, head_, html_, id_, input_, method_, name_, oninput_, onsubmit_, script_, table_, td_, title_, toHtml, tr_, type_)
import Web.FormUrlEncoded (FromForm (..), parseUnique)

data Form where
  Form :: {greeting :: Text} -> Form
  deriving stock (Generic)

instance FromForm Form where
  fromForm f = Form <$> parseUnique "greeting" f

table :: [String] -> Html ()
table gs = table_ $ mapM_ (tr_ . td_ . toHtml) gs

input :: Html ()
input = form_ [method_ "post", action_ "/add-greeting", onsubmit_ "return validateForm()"] $ do
  input_ [type_ "text", id_ "greetingInput", name_ "greeting", oninput_ "validateInput()"]
  button_ [type_ "submit", id_ "submitButton", disabled_ "disabled"] "Add Greeting"
  script_
    "function validateInput() {\
    \  var input = document.getElementById('greetingInput').value;\
    \  var button = document.getElementById('submitButton');\
    \  if (input.trim() === '') {\
    \    button.disabled = true;\
    \  } else {\
    \    button.disabled = false;\
    \  }\
    \}\
    \function validateForm() {\
    \  var input = document.getElementById('greetingInput').value;\
    \  if (input.trim() === '') {\
    \    alert('Greeting cannot be empty!');\
    \    return false;\
    \  }\
    \  return true;\
    \}"

get :: (MonadIO m) => DB.Environment -> m (Html ())
get env = do
  greetings <- liftIO $ DB.queryGreetings env
  return $ html_ $ do
    head_ $ title_ "Greetings Table"
    body_ $ do
      h1_ "List of Greetings"
      table greetings
      input

add :: (MonadIO m) => DB.Environment -> Form -> m ()
add env (Form newG) = do
  liftIO $ DB.insertGreeting env $ T.unpack newG
