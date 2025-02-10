{-# LANGUAGE TypeFamilies #-}

module Login.Session (Environment, generateEnv, headers, authContext) where

import Control.Monad (replicateM, (<=<))
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 (encodeBase64)
import Data.Either.Extra (eitherToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T (decodeUtf8', encodeUtf8)
import Debug.Trace qualified as Debug
import Login.User (User)
import Network.HTTP.Types (Header)
import Network.HTTP.Types.Header (hContentType, hSetCookie)
import Network.Wai (Request, requestHeaders)
import Servant (AuthProtect, Context (EmptyContext, (:.)), err401, errBody, errHeaders, throwError)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import System.Random (genWord8, getStdRandom)
import Web.Cookie (SetCookie (..), defaultSetCookie, parseCookies, renderSetCookieBS, sameSiteStrict, setCookieName, setCookiePath, setCookieValue)
import Web.JWT (JWT, JWTClaimsSet (iss, sub), VerifiedJWT)
import Web.JWT qualified as JWT

newtype Environment = Env {secretKey :: SecretKey}

instance Show Environment where
  show Env {secretKey = key} = show $ encodeBase64 key

type instance AuthServerData (AuthProtect "jwt-auth") = JWT VerifiedJWT

type SecretKey = BS.ByteString

generateEnv :: (MonadIO m) => m Environment
generateEnv = Env <$> generateSecretKey

generateSecretKey :: (MonadIO m) => m SecretKey
generateSecretKey =
  BS.pack <$> replicateM 32 (getStdRandom genWord8)

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
          { iss = JWT.stringOrURI "wv2.hopTo.org",
            sub = JWT.stringOrURI $ T.show user
          }
      key = JWT.EncodeHMACSecret $ secretKey env
   in JWT.encodeSigned key mempty claimsSet

authContext :: Environment -> Context (AuthHandler Request (JWT VerifiedJWT) ': '[])
authContext env = jwtAuthHandler env :. EmptyContext

verifyJWT :: Environment -> BS.ByteString -> Maybe (JWT VerifiedJWT)
verifyJWT env =
  JWT.decodeAndVerifySignature (JWT.VerifyHMACSecret $ secretKey env) <=< eitherToMaybe . T.decodeUtf8'

extractToken :: Request -> Maybe BS.ByteString
extractToken req = do
  cookieHeader <- Debug.traceShowId $ lookup "Cookie" (Debug.traceShowId $ requestHeaders req)
  Debug.traceShowId $ lookup "authToken" $ Debug.traceShowId $ parseCookies cookieHeader

jwtAuthHandler :: Environment -> AuthHandler Request (JWT VerifiedJWT)
jwtAuthHandler env = mkAuthHandler handler
  where
    handler req = case extractToken req >>= verifyJWT env of
      Just jwt -> return jwt
      Nothing -> throwError err401 {errBody = msg, errHeaders = [("Content-Type", "text/html")]} -- TODO: Add a login link
        where
          msg = "<html><body><h1>Unauthorized</h1><p>You need to log in to access this page.</p></body></html>"

createJWTSetCookie :: Text -> SetCookie
createJWTSetCookie jwtToken =
  defaultSetCookie
    { setCookieName = "authToken",
      setCookieValue = T.encodeUtf8 jwtToken,
      setCookiePath = Just "/",
      setCookieHttpOnly = True, -- Prevents JavaScript access
      setCookieSecure = True, -- Ensures the cookie is sent over HTTPS
      setCookieSameSite = Just sameSiteStrict -- sameSiteLax -- Helps mitigate cross-site request forgery (CSRF) attacks
      -- Optionally set expiration
      -- , setCookieExpires = Just someExpirationTime
    }
