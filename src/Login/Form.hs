module Login.Form (Form (..), page, GetPage, PageHandler) where

import Control.Monad.Extra (whenJust)
import Data.Text (Text)
import Lucid (Html, action_, body_, br_, doctype_, form_, h1_, head_, html_, input_, label_, method_, name_, p_, style_, title_, type_, value_)
import Servant (Get, Handler, QueryParam, (:>))
import Servant.HTML.Lucid
import Util.Redirect (Reason (BadCredentials, NeedToLogIn))
import Web.FormUrlEncoded (FromForm (..), parseUnique)

type GetPage = QueryParam "reason" Reason :> Get '[HTML] (Html ())

type PageHandler = Maybe Reason -> Handler (Html ())

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

page :: PageHandler
page reason_ = return $ do
  doctype_
  html_ $ do
    head_ $ title_ "Login Page"
    body_ $ do
      h1_ "Login"
      whenJust reason_ $
        p_ [style_ "color:red;"] . \case
          NeedToLogIn -> "Unauthorized - You need to login before doing that."
          BadCredentials -> "Invalid credentials - Please try again."
      form_ [method_ "post", action_ "/login"] $ do
        label_ "Username: "
        input_ [type_ "text", name_ "username"]
        br_ []
        label_ "Password: "
        input_ [type_ "password", name_ "password"]
        br_ []
        input_ [type_ "submit", value_ "Login"]
