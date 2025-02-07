{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Login (Endpoints, handlers) where

import Data.Text (Text)
import Lucid (Html, action_, body_, br_, doctype_, form_, h1_, head_, html_, input_, label_, method_, name_, title_, type_, value_)
import Servant (FormUrlEncoded, Get, Handler, NoContent (..), Post, ReqBody, err401, errBody, throwError, (:<|>) (..), (:>))
import Servant.HTML.Lucid (HTML)
import Util.Redirect (redirectTo)
import Web.FormUrlEncoded (FromForm (..), parseUnique)

data Form = LoginForm
  { username :: Text,
    password :: Text
  }

instance FromForm Form where
  fromForm f =
    LoginForm
      <$> parseUnique "username" f
      <*> parseUnique "password" f

type Endpoints = GetPage :<|> PostRequest

type GetPage = Get '[HTML] (Html ())

type PostRequest = ReqBody '[FormUrlEncoded] Form :> Post '[HTML] NoContent

handlers :: Text -> Handler (Html ()) :<|> (Form -> Handler NoContent)
handlers successPath = page :<|> \form -> handler form >> redirectTo successPath

handler :: Form -> Handler NoContent
handler form = do
  if authenticate (username form) (password form)
    then return NoContent
    else throwError err401 {errBody = "Invalid credentials"}

authenticate :: Text -> Text -> Bool
authenticate user pass =
  user == "admin" && pass == "password" -- Simple hardcoded check

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
