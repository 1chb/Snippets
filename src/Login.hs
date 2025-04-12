module Login (Endpoints, handlers) where

import Login.Form (Form, GetPage, PageHandler, page)
import Servant (FormUrlEncoded, Handler, NoContent, Post, ReqBody, (:<|>) (..), (:>))
import Servant.HTML.Lucid (HTML)
import Session (Environment, headers)
import User (authenticate)
import Util.Redirect (LoginReason (BadCredentials), Path (Login, Main), redirectTo)
import qualified Debug.Trace as Debug

type Endpoints = GetPage :<|> PostRequest

type PostRequest = ReqBody '[FormUrlEncoded] Form :> Post '[HTML] NoContent

handlers :: Environment -> PageHandler :<|> FormHandler
handlers env = page :<|> handler env

type FormHandler = Form -> Handler NoContent

handler :: Environment -> FormHandler
handler env form = do
  case authenticate (Debug.trace "*** Login.handler ***" env) $ Debug.traceShowId form of
    Just user ->
      redirectTo (headers env user) Main
    Nothing ->
      redirectTo [] . Login $ Just BadCredentials
