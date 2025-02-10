module Login.User (User, authenticate) where

import Data.Text (Text)
import Login.Form (Form (..))

data User = User {name :: Text, privileges :: Privileges} deriving stock (Show)

data Privileges = Admin deriving stock (Show)

authenticate :: Form -> Maybe User
authenticate form =
  if form == LoginForm "admin" "password" -- Simple hardcoded check
    then Just $ User {name = username form, privileges = Admin}
    else Nothing
