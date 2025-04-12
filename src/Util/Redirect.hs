module Util.Redirect
  ( redirectTo,
    Path (..),
    LoginReason (..),
    loginReason,
    compile,
    Login,
    Greetings,
    VerifyLoggedOut,
  )
where

import Control.Monad.Error.Class (MonadError)
import Data.Proxy (Proxy (..))
import Data.String (IsString (fromString))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Network.HTTP.Types (Header, hLocation)
import Servant (FromHttpApiData (..), ServerError, err303, errHeaders, throwError)

data Path
  = Login (Maybe LoginReason)
  | VerifyLoggedOut
  | Main

data LoginReason = NeedToLogIn | BadCredentials | LoggedOut
  deriving stock (Eq)

type Login = "login"

type Greetings = "greetings"

type VerifyLoggedOut = "--verify-logged-out"

loginReason :: (IsString s, Semigroup s) => LoginReason -> s
loginReason =
  ("reason=" <>) . \case
    NeedToLogIn -> "1"
    BadCredentials -> "2"
    LoggedOut -> "3"

instance FromHttpApiData LoginReason where
  parseUrlPiece = \case
    "1" -> pure NeedToLogIn
    "2" -> pure BadCredentials
    "3" -> pure LoggedOut
    _ -> Left "BadRequest"

compile :: (IsString str, Semigroup str) => Path -> str
compile = \case
  Login mReason -> "/" <> str @Login Proxy <> maybe "" (("?" <>) . loginReason) mReason
  VerifyLoggedOut -> "/" <> str @VerifyLoggedOut Proxy
  Main -> "/" <> str @Greetings Proxy
  where
    str :: (KnownSymbol typ, IsString str) => Proxy typ -> str
    str = fromString . symbolVal

redirectTo :: (MonadError ServerError m) => [Header] -> Path -> m a
redirectTo extraHeaders path =
  throwError err303 {errHeaders = (hLocation, compile path) : extraHeaders}
