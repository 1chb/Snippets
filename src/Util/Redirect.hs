{-# LANGUAGE OverloadedStrings #-}

module Util.Redirect (redirectTo) where

import Data.ByteString (ByteString)
import Servant (Handler, NoContent, err303, errHeaders, throwError)

redirectTo :: ByteString -> Handler NoContent
redirectTo path = throwError err303 {errHeaders = [("Location", path)]}
