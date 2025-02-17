module Api (api, server) where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Database qualified as DB
import Greeting qualified
import Login qualified
import Paths (Paths (Login), path)
import Robot qualified
import Servant (AuthProtect, Get, Header, Headers (..), JSON, NoContent (..), OctetStream, PlainText, Proxy (..), RemoteHost, Server, addHeader, (:<|>) (..), (:>))
import Servant.HTML.Lucid (HTML)
import Session qualified
import Util.Redirect (redirectTo)

type API =
  "favicon.ico" :> Get '[OctetStream] NoContent
    :<|> Robot.Api
    :<|> Get '[HTML] NoContent -- Root path redirect
    :<|> "login" :> Login.Endpoints
    :<|> AuthProtect "jwt-auth" :> "greeting" :> Greeting.Endpoints
    :<|> "old" :> Get '[PlainText] String
    :<|> "hello" :> Hellos
    :<|> "users" :> Get '[JSON] (Headers '[Header "User-Count" Integer] [String])

type Hellos =
  Header "User-Agent" String :> RemoteHost :> Get '[PlainText] String
    :<|> "there" :> Get '[PlainText] String

server :: DB.Environment -> Session.Environment -> Server API
server dbEnv sessionEnv =
  getFavicon
    :<|> Robot.handler
    :<|> redirectTo [] (path Login)
    :<|> Login.handlers sessionEnv
    :<|> protected (Greeting.handlers dbEnv)
    :<|> (concat <$> liftIO (DB.queryGreetings dbEnv))
    :<|> fmap ("Hello, " <>) <$> hellos -- Only hello to snd!
    :<|> return (addHeader 2 ["X", "Y"])
  where
    getFavicon = return NoContent
    hellos = (\a b -> return $ fromMaybe "Unknown agent" a <> "\n" <> show b) :<|> return "there!"
    protected handler _VerifiedJWT = handler

api :: Proxy API
api = Proxy
