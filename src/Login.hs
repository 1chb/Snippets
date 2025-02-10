module Login (Endpoints, handlers) where

import Data.Text (Text)
import Login.Form (Form, page)
import Login.Session (Environment, headers)
import Login.User (authenticate)
import Lucid (Html)
import Servant (FormUrlEncoded, Get, Handler, NoContent, Post, ReqBody, err401, errBody, throwError, (:<|>) (..), (:>))
import Servant.HTML.Lucid (HTML)
import Util.Redirect (redirectTo)

type Endpoints = GetPage :<|> PostRequest

type GetPage = Get '[HTML] (Html ())

type PostRequest = ReqBody '[FormUrlEncoded] Form :> Post '[HTML] NoContent

handlers :: Environment -> Text -> Handler (Html ()) :<|> (Form -> Handler NoContent)
handlers env successPath = page :<|> handler env successPath

handler :: Environment -> Text -> Form -> Handler NoContent
handler env redirectPath form = do
  case authenticate form of
    Just user ->
      redirectTo (headers env user) redirectPath
    Nothing ->
      throwError err401 {errBody = "Invalid credentials"}
