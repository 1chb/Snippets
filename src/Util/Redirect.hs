module Util.Redirect (redirectTo) where

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (Header, hLocation)
import Servant (Handler, NoContent, err303, errHeaders, throwError)

redirectTo :: [Header] -> Text -> Handler NoContent
redirectTo extraHeaders path =
  throwError err303 {errHeaders = (hLocation, encodeUtf8 path) : extraHeaders}
