{-# LANGUAGE OverloadedStrings #-}

module Chess.OpeningTable.OpeningTablePostgres where

import Control.Monad.Logger
import Control.Monad.Reader
import Database.Persist.Postgresql

connString :: ConnectionString
connString = "host=postgres port=5432 user=postgres dbname=postgres password=postgres"

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action =
  runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend -> runReaderT action backend
