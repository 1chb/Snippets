{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}

module User (User, AdminPwd, authenticate, userName) where

import Config (AdminPwd (..), Environment (adminPwd))
import Data.Text (Text)
import Login.Form (Form (..))
import GHC.Generics (Generic)
import qualified Data.Aeson.Types as Aeson

data User = User {name :: Text, privileges :: Privileges}
  deriving stock (Generic, Show)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data Privileges = Admin
  deriving stock (Generic, Show)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

authenticate :: Environment -> Form -> Maybe User
authenticate (adminPwd -> AdminPwd aPwd) form =
  if form == LoginForm "admin" aPwd -- Simple hardcoded check
    then Just $ User {name = username form, privileges = Admin}
    else Nothing

userName :: User -> Text
userName = name