module Api (api, server) where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Database qualified as DB
import Greeting qualified
import Login qualified
import Login.Session qualified as Session
import Servant (Get, Header, Headers (..), JSON, NoContent (..), OctetStream, PlainText, Proxy (..), Server, addHeader, (:<|>) (..), (:>))
import Servant.HTML.Lucid (HTML)
import Util.Redirect (redirectTo)

type API =
  "favicon.ico" :> Get '[OctetStream] NoContent
    :<|> Get '[HTML] NoContent -- Root path redirect
    :<|> "login" :> Login.Endpoints
    :<|> "greeting" :> Greeting.Endpoints
    :<|> "old" :> Get '[PlainText] String
    :<|> "hello" :> Hellos
    :<|> "users" :> Get '[JSON] (Headers '[Header "User-Count" Integer] [String])

type Hellos =
  Header "User-Agent" String :> Get '[PlainText] String
    :<|> "there" :> Get '[PlainText] String

server :: DB.Environment -> Session.Environment -> Server API
server dbEnv _sessionEnv =
  getFavicon
    :<|> redirectTo "/login"
    :<|> Login.handlers "/greeting"
    :<|> Greeting.handlers dbEnv "/greeting"
    :<|> (concat <$> liftIO (DB.queryGreetings dbEnv))
    :<|> fmap ("Hello, " <>) <$> hellos -- Only hello to snd!
    :<|> return (addHeader 2 ["X", "Y"])
  where
    getFavicon = return NoContent
    hellos = return . fromMaybe "???" :<|> return "there!"

api :: Proxy API
api = Proxy
