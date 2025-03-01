{-# LANGUAGE TypeFamilies #-}

module Session (Environment (..), generateSecretKey, headers, authContext, justProtect) where

import Config (Environment (Env, secretKey), SecretKey)
import Control.Monad (replicateM, (<=<))
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Either.Extra (eitherToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Types (Header)
import Network.HTTP.Types.Header (hContentType, hSetCookie)
import Network.Wai (Request (rawPathInfo), requestHeaders)
import Servant (AuthProtect, Context (EmptyContext, (:.)))
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import Session.Cookie qualified
import System.Random (genWord8, getStdRandom)
import User (User)
import Util.Redirect (LoginReason (LoggedOut, NeedToLogIn), Path (Login, VerifyLoggedOut), compile, redirectTo)
import Web.Cookie (parseCookies, renderSetCookieBS)
import Web.JWT (JWT, JWTClaimsSet (iss, sub), VerifiedJWT)
import Web.JWT qualified as JWT

type instance AuthServerData (AuthProtect "jwt-auth") = User

authContext :: Environment -> Context (AuthHandler Request User ': '[])
authContext env = jwtAuthHandler env :. EmptyContext

jwtAuthHandler :: Environment -> AuthHandler Request User
jwtAuthHandler env = mkAuthHandler handler
  where
    handler req = case extractToken req >>= verifyJWT env >>= extractUser . JWT.claims of
      Just user -> return user
      Nothing ->
        redirectTo [] . Login $
          Just $
            if rawPathInfo req == compile VerifyLoggedOut then LoggedOut else NeedToLogIn

generateSecretKey :: (MonadIO m) => m SecretKey
generateSecretKey =
  BS.pack <$> replicateM 32 (getStdRandom genWord8)

headers :: Environment -> User -> [Header]
headers env user =
  let cookie = Session.Cookie.createJWTSet $ generateJWT env user
   in [ (hSetCookie, renderSetCookieBS cookie),
        (hContentType, "text/plain")
      ]

generateJWT :: Environment -> User -> Text
generateJWT env user =
  let claimsSet =
        mempty
          { iss = JWT.stringOrURI "wv2.hopTo.org",
            sub = JWT.stringOrURI $ encodeToStrictText user
          }
      key = JWT.EncodeHMACSecret $ secretKey env
   in JWT.encodeSigned key mempty claimsSet

verifyJWT :: Environment -> BS.ByteString -> Maybe (JWT VerifiedJWT)
verifyJWT env =
  JWT.decodeAndVerifySignature (JWT.VerifyHMACSecret $ secretKey env) <=< eitherToMaybe . TE.decodeUtf8'

extractToken :: Request -> Maybe BS.ByteString
extractToken req = do
  cookieHeader <- lookup "Cookie" (requestHeaders req)
  lookup "authToken" $ parseCookies cookieHeader

extractUser :: JWT.JWTClaimsSet -> Maybe User
extractUser claims = do
  sub <- JWT.sub claims
  Aeson.decodeStrictText $ JWT.stringOrURIToText sub

justProtect :: api -> User -> api
justProtect handler _user = handler

-- This throws an exception if the intermediate byte sequence is not UTF8, but we trust Aeson to do the right thing...
encodeToStrictText :: (Aeson.ToJSON a) => a -> T.Text
encodeToStrictText = TE.decodeUtf8 . BS.toStrict . Aeson.encode
