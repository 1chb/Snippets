module Util.Redirect
  ( redirectTo,
    Path (..),
    Reason (..),
    reason,
    compile,
    Login,
    Greetings,
  )
where

import Control.Monad.Error.Class (MonadError)
import Data.String (IsString)
import Network.HTTP.Types (Header, hLocation)
import Servant (FromHttpApiData (..), ServerError, err303, errHeaders, throwError)

data Path
  = Login (Maybe Reason)
  | Main

data Reason = NeedToLogIn | BadCredentials

reason :: (IsString s, Semigroup s) => Reason -> s
reason =
  ("reason=" <>) . \case
    NeedToLogIn -> "1"
    BadCredentials -> "2"

instance FromHttpApiData Reason where
  parseUrlPiece = \case
    "1" -> pure NeedToLogIn
    "2" -> pure BadCredentials
    _ -> Left "BadRequest"

compile :: (IsString s, Semigroup s) => Path -> s
compile = \case
  Login mReason -> "/" <> login <> maybe "" (("?" <>) . reason) mReason
  Main -> "/" <> greetings

type Login = "login"

login :: (IsString s) => s
login = "login"

type Greetings = "greetings"

greetings :: (IsString s) => s
greetings = "greetings"

redirectTo :: (MonadError ServerError m) => [Header] -> Path -> m a
redirectTo extraHeaders path =
  throwError err303 {errHeaders = (hLocation, compile path) : extraHeaders}
