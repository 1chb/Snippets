module Session.Cookie (createJWTSetCookie) where

import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Web.Cookie (SetCookie (..), defaultSetCookie, sameSiteStrict, setCookieName, setCookiePath, setCookieValue)

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
