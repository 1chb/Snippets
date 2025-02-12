{-# LANGUAGE ViewPatterns #-}

module User (User, AdminPwd, authenticate) where

import Config (AdminPwd (..), Environment (adminPwd))
import Data.Text (Text)
import Login.Form (Form (..))

data User = User {name :: Text, privileges :: Privileges} deriving stock (Show)

data Privileges = Admin deriving stock (Show)

authenticate :: Environment -> Form -> Maybe User
authenticate (adminPwd -> AdminPwd aPwd) form =
  if form == LoginForm "admin" aPwd -- Simple hardcoded check
    then Just $ User {name = username form, privileges = Admin}
    else Nothing
