{-# LANGUAGE OverloadedStrings #-}

module Database (runDb, queryGreetings, withDbConnection) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist (Entity (..), selectList)
import Database.Persist.Postgresql (ConnectionPool, ConnectionString, SqlBackend, createPostgresqlPool, runMigration, runSqlPool)
import Model (Greeting (..), migrateAll)

-- Connection string for PostgreSQL
connStr :: ConnectionString
connStr = "host=localhost dbname=mydb user=myuser password=mypassword port=5432"

-- Create a connection pool
withDbConnection :: (MonadUnliftIO m) => (ConnectionPool -> LoggingT m a) -> m a
withDbConnection action = runStdoutLoggingT $ do
  pool <- createPostgresqlPool connStr 10
  runSqlPool (runMigration migrateAll) pool
  action pool

-- Run a database action
runDb :: (MonadUnliftIO m) => ConnectionPool -> ReaderT SqlBackend m a -> m a
runDb pool action = runSqlPool action pool

-- Query greetings from the database
queryGreetings :: (MonadUnliftIO m) => ConnectionPool -> m [String]
queryGreetings pool = runDb pool $ do
  greetings <- selectList [] []
  return $ map (\(Entity _ (Greeting msg)) -> msg) greetings
