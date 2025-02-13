module Robot (Api, handler) where

import Data.Text (Text)
import Data.Text qualified as T
import Servant (Get, Handler, PlainText, (:>))

type Api = "robots.txt" :> Get '[PlainText] Result

type Result = Text

handler :: Handler Result
handler =
  return $
    T.unlines
      [ "User-agent: *",
        "Disallow: /"
      ]
