module Login.Session (Environment, generateEnv, headers) where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (MonadIO)
import Data.Base64.Types (Alphabet (StdPadded), Base64, extractBase64)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 (encodeBase64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Login.User (User)
import Network.HTTP.Types (Header)
import Network.HTTP.Types.Header (hContentType, hSetCookie)
import System.Random (genWord8, getStdRandom)
import Web.Cookie (SetCookie (..), defaultSetCookie, renderSetCookieBS, sameSiteStrict, setCookieName, setCookiePath, setCookieValue)
import Web.JWT (JWTClaimsSet (iss, sub), encodeSigned, hmacSecret, stringOrURI)

newtype Environment = Env {secretKey :: SecretKey} deriving stock (Show)

type SecretKey = Base64 'StdPadded Text

generateEnv :: (MonadIO m) => m Environment
generateEnv = Env <$> generateSecretKey

generateSecretKey :: (MonadIO m) => m SecretKey
generateSecretKey =
  encodeBase64 . BS.pack <$> replicateM 32 (getStdRandom genWord8)

headers :: Environment -> User -> [Header]
headers env user =
  let cookie = createJWTSetCookie $ generateJWT env user
   in [ (hSetCookie, renderSetCookieBS cookie),
        (hContentType, "text/plain")
      ]

generateJWT :: Environment -> User -> Text
generateJWT env user =
  let claimsSet =
        mempty
          { iss = stringOrURI "wv2.hopTo.org",
            sub = stringOrURI $ T.show user
          }
      key = hmacSecret . extractBase64 $ secretKey env
   in encodeSigned key mempty claimsSet

createJWTSetCookie :: Text -> SetCookie
createJWTSetCookie jwtToken =
  defaultSetCookie
    { setCookieName = "authToken",
      setCookieValue = encodeUtf8 jwtToken,
      setCookiePath = Just "/",
      setCookieHttpOnly = True, -- Prevents JavaScript access
      setCookieSecure = True, -- Ensures the cookie is sent over HTTPS
      setCookieSameSite = Just sameSiteStrict -- sameSiteLax -- Helps mitigate cross-site request forgery (CSRF) attacks
      -- Optionally set expiration
      -- , setCookieExpires = Just someExpirationTime
    }
