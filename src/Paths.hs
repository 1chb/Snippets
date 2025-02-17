module Paths (Paths (..), path) where

import Data.Text (Text)

data Paths
  = Login
  | Main

path :: Paths -> Text
path = \case
  Login -> "/login"
  Main -> "/greeting"
