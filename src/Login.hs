module Login (Endpoints, handlers) where

import Login.Form (Form, GetPage, PageHandler, Reason (BadCredentials), page, reason)
import Paths (Paths (Login, Main), path)
import Servant (FormUrlEncoded, Handler, NoContent, Post, ReqBody, (:<|>) (..), (:>))
import Servant.HTML.Lucid (HTML)
import Session (Environment, headers)
import User (authenticate)
import Util.Redirect (redirectTo)

type Endpoints = GetPage :<|> PostRequest

type PostRequest = ReqBody '[FormUrlEncoded] Form :> Post '[HTML] NoContent

handlers :: Environment -> PageHandler :<|> FormHandler
handlers env = page :<|> handler env

type FormHandler = Form -> Handler NoContent

handler :: Environment -> FormHandler
handler env form = do
  case authenticate env form of
    Just user ->
      redirectTo (headers env user) $ path Main
    Nothing ->
      redirectTo [] $ path Login <> "?" <> reason BadCredentials
