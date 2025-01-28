{-# LANGUAGE OverloadedStrings #-}

module Database (runDb, queryGreetings, withDbConnection) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist (Entity (..), selectList)
import Database.Persist.Postgresql (ConnectionPool, ConnectionString, SqlBackend, createPostgresqlPool, runMigration, runSqlPool)
import Model (Greeting (..), migrateAll)

-- Connection string for PostgreSQL
connStr :: ConnectionString
connStr = "host=localhost dbname=mydb user=myuser password=mypassword port=5432"

-- Create a connection pool
withDbConnection :: (ConnectionPool -> IO a) -> IO a
withDbConnection action = runStdoutLoggingT $ do
  pool <- createPostgresqlPool connStr 10
  liftIO $ runSqlPool (runMigration migrateAll) pool
  liftIO $ action pool

-- Run a database action
runDb :: ConnectionPool -> ReaderT SqlBackend IO a -> IO a
runDb pool action = runSqlPool action pool

-- Query greetings from the database
queryGreetings :: ConnectionPool -> IO [String]
queryGreetings pool = runDb pool $ do
  greetings <- selectList [] []
  return $ map (\(Entity _ (Greeting msg)) -> msg) greetings
