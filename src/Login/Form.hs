module Login.Form (Form (..), page) where

import Data.Text (Text)
import Lucid (Html, action_, body_, br_, doctype_, form_, h1_, head_, html_, input_, label_, method_, name_, title_, type_, value_)
import Servant (Handler)
import Web.FormUrlEncoded (FromForm (..), parseUnique)

data Form = LoginForm
  { username :: Text,
    _password :: Text
  }
  deriving stock (Eq)

instance FromForm Form where
  fromForm f =
    LoginForm
      <$> parseUnique "username" f
      <*> parseUnique "password" f

page :: Handler (Html ())
page = return $ do
  doctype_
  html_ $ do
    head_ $ title_ "Login Page"
    body_ $ do
      h1_ "Login"
      form_ [method_ "post", action_ "/login"] $ do
        label_ "Username: "
        input_ [type_ "text", name_ "username"]
        br_ []
        label_ "Password: "
        input_ [type_ "password", name_ "password"]
        br_ []
        input_ [type_ "submit", value_ "Login"]
