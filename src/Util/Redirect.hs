module Util.Redirect (redirectTo) where

import Data.ByteString (ByteString)
import Data.Tagged (Tagged, untag)
import Data.Text (Text)
import Servant (Handler, NoContent, err303, errHeaders, throwError)
import Witch qualified

redirectTo :: Text -> Handler NoContent
redirectTo path =
  throwError err303 {errHeaders = [("Location", untag . Witch.into @(Tagged "UTF-8" ByteString) $ path)]}
