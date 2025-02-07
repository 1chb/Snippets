{-# LANGUAGE OverloadedStrings #-}

module Login (page) where

import Lucid (Html, action_, body_, br_, doctype_, form_, h1_, head_, html_, input_, label_, method_, name_, title_, type_, value_)
import Servant (Handler)

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
