{-# LANGUAGE TypeFamilies #-}

module Session (Environment (..), generateSecretKey, headers, authContext) where

import Config (Environment (Env, secretKey), SecretKey)
import Control.Monad (replicateM, (<=<))
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString qualified as BS
import Data.Either.Extra (eitherToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T (decodeUtf8')
import Network.HTTP.Types (Header)
import Network.HTTP.Types.Header (hContentType, hSetCookie)
import Network.Wai (Request, requestHeaders)
import Servant (AuthProtect, Context (EmptyContext, (:.)), err401, errBody, errHeaders, throwError)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import Session.Cookie (createJWTSetCookie)
import System.Random (genWord8, getStdRandom)
import User (User)
import Web.Cookie (parseCookies, renderSetCookieBS)
import Web.JWT (JWT, JWTClaimsSet (iss, sub), VerifiedJWT)
import Web.JWT qualified as JWT

type instance AuthServerData (AuthProtect "jwt-auth") = JWT VerifiedJWT

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
  cookieHeader <- lookup "Cookie" (requestHeaders req)
  lookup "authToken" $ parseCookies cookieHeader

jwtAuthHandler :: Environment -> AuthHandler Request (JWT VerifiedJWT)
jwtAuthHandler env = mkAuthHandler handler
  where
    handler req = case extractToken req >>= verifyJWT env of
      Just jwt -> return jwt
      Nothing -> throwError err401 {errBody = msg, errHeaders = [("Content-Type", "text/html")]} -- TODO: Add a login link
        where
          msg = "<html><body><h1>Unauthorized</h1><p>You need to log in to access this page.</p></body></html>"
