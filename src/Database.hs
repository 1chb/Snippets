module Database (Environment, runDb, queryGreetings, connectAndMigrate, insertGreeting) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist (Entity (..), insert_, selectList)
import Database.Persist.Postgresql (ConnectionPool, ConnectionString, SqlBackend, createPostgresqlPool, runMigration, runSqlPool)
import Model (Greeting (..), migrateAll)

connStr :: ConnectionString
connStr = "host=localhost dbname=mydb user=myuser password=mypassword port=5432"

connectAndMigrate :: IO Environment
connectAndMigrate = do
  pool <- runStdoutLoggingT $ createPostgresqlPool connStr 10
  runSqlPool (runMigration migrateAll) pool
  pure pool

type Environment = ConnectionPool

runDb :: (MonadUnliftIO m) => Environment -> ReaderT SqlBackend m a -> m a
runDb pool action = runSqlPool action pool

queryGreetings :: (MonadUnliftIO m) => Environment -> m [String]
queryGreetings pool = runDb pool $ do
  greetings <- selectList [] []
  return $ map (\(Entity _ (Greeting msg)) -> msg) greetings

insertGreeting :: (MonadUnliftIO m) => Environment -> String -> m ()
insertGreeting pool msg = runDb pool $ do
  insert_ $ Greeting msg
