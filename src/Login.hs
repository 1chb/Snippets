module Login (Endpoints, handlers) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Login.Form (Form, page)
import Login.Session (Environment, generateJWT)
import Login.User (authenticate)
import Lucid (Html)
import Servant (FormUrlEncoded, Get, Handler, NoContent (..), Post, ReqBody, err401, errBody, throwError, (:<|>) (..), (:>))
import Servant.HTML.Lucid (HTML)
import Util.Redirect (redirectTo)

type Endpoints = GetPage :<|> PostRequest

type GetPage = Get '[HTML] (Html ())

type PostRequest = ReqBody '[FormUrlEncoded] Form :> Post '[HTML] NoContent

handlers :: Environment -> Text -> Handler (Html ()) :<|> (Form -> Handler NoContent)
handlers env successPath = page :<|> \form -> handler env form >> redirectTo successPath

handler :: Environment -> Form -> Handler NoContent
handler env form = do
  case authenticate form of
    Just user -> do
      let jwt = generateJWT env user
      liftIO $ print jwt
      return NoContent
    Nothing ->
      throwError err401 {errBody = "Invalid credentials"}
