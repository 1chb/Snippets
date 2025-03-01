module Session.Cookie (createJWTSet, deleteJWTSet) where

import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Web.Cookie (SetCookie (..), defaultSetCookie, sameSiteStrict, setCookieName, setCookiePath, setCookieValue)

createJWTSet :: Text -> SetCookie
createJWTSet jwtToken =
  jwtSet
    { setCookieValue = T.encodeUtf8 jwtToken
    -- Optionally set expiration
    -- , setCookieExpires = Just someExpirationTime
    }

deleteJWTSet :: SetCookie
deleteJWTSet =
  jwtSet
    { setCookieValue = "",
      setCookieExpires = Just $ addUTCTime (-3600) (UTCTime (fromGregorian 1970 1 1) 0)
    }

jwtSet :: SetCookie
jwtSet =
  defaultSetCookie
    { setCookieName = "authToken",
      setCookiePath = Just "/",
      setCookieHttpOnly = True, -- Prevents JavaScript access
      setCookieSecure = True, -- Ensures the cookie is sent over HTTPS
      setCookieSameSite = Just sameSiteStrict -- sameSiteLax -- Helps mitigate cross-site request forgery (CSRF) attacks
    }
