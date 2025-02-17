module Greeting (Endpoints, handlers) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Tagged ()
import Data.Text (Text)
import Data.Text qualified as T
import Database qualified as DB
import GHC.Generics (Generic)
import Lucid (Html, action_, body_, button_, disabled_, form_, h1_, head_, html_, id_, input_, method_, name_, oninput_, onsubmit_, script_, table_, td_, title_, toHtml, tr_, type_)
import Servant (FormUrlEncoded, Get, Handler, NoContent, Post, ReqBody, (:<|>) (..), (:>))
import Servant.HTML.Lucid (HTML)
import Util.Redirect (Path (Main), compile, redirectTo)
import Web.FormUrlEncoded (FromForm (..), parseUnique)

data Form where
  Form :: {greeting :: Text} -> Form
  deriving stock (Generic)

instance FromForm Form where
  fromForm f = Form <$> parseUnique greetingField f

greetingField :: Text
greetingField = "greeting"

type Endpoints = GetPage :<|> AddRequest

type GetPage = Get '[HTML] (Html ())

type AddRequest = "add" :> ReqBody '[FormUrlEncoded] Greeting.Form :> Post '[HTML] NoContent

handlers :: DB.Environment -> Handler (Html ()) :<|> (Form -> Handler NoContent)
handlers env = get env :<|> \form -> Greeting.add env form >> redirectTo [] Main -- TODO ?

table :: [String] -> Html ()
table gs = table_ $ mapM_ (tr_ . td_ . toHtml) gs

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

input :: Html ()
input = form_ [method_ "post", action_ $ compile Main <> "/add", onsubmit_ "return validateForm()"] $ do
  input_ [type_ "text", id_ "greetingInput", name_ greetingField, oninput_ "validateInput()"]
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
