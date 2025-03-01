module Api (api, app) where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Database qualified as DB
import Greeting qualified
import Login qualified
import Logout qualified
import Robot qualified
import Servant (AuthProtect, Get, Header, Headers (..), JSON, NoContent (..), OctetStream, PlainText, Proxy (..), (:<|>) (..), (:>))
import Servant qualified
import Servant.HTML.Lucid (HTML)
import Session (authContext)
import Session qualified
import Util.Redirect (Greetings, Login, Path (Login), redirectTo)

type API =
  "favicon.ico" :> Get '[OctetStream] NoContent
    :<|> Robot.Api
    :<|> Get '[HTML] NoContent -- Root path redirect
    :<|> Login :> Login.Endpoints
    :<|> AuthProtect "jwt-auth" :> Logout.Endpoints
    :<|> AuthProtect "jwt-auth" :> Greetings :> Greeting.Endpoints
    :<|> "old" :> Get '[PlainText] String
    :<|> "hello" :> Hellos
    :<|> "users" :> Get '[JSON] (Headers '[Header "User-Count" Integer] [String])

type Hellos =
  Header "User-Agent" String :> Servant.RemoteHost :> Get '[PlainText] String
    :<|> "there" :> Get '[PlainText] String

server :: DB.Environment -> Session.Environment -> Servant.Server API
server dbEnv sessionEnv =
  getFavicon
    :<|> Robot.handler
    :<|> redirectTo [] (Login Nothing)
    :<|> Login.handlers sessionEnv
    :<|> Session.justProtect Logout.handlers
    :<|> Greeting.handlers dbEnv
    :<|> (concat <$> liftIO (DB.queryGreetings dbEnv))
    :<|> fmap ("Hello, " <>) <$> hellos -- Only hello to snd!
    :<|> return (Servant.addHeader 2 ["X", "Y"])
  where
    getFavicon = return NoContent
    hellos = (\a b -> return $ fromMaybe "Unknown agent" a <> "\n" <> show b) :<|> return "there!"

api :: Proxy API
api = Proxy

app :: DB.Environment -> Session.Environment -> Servant.Application
app dbEnv env = Servant.serveWithContext (Proxy :: Proxy API) (authContext env) (server dbEnv env)
