{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chess.OpeningTable.OpeningTableReader where

import Chess.OpeningTable.OpeningTableAccessor
import Chess.OpeningTable.OpeningTableInserter
import Chess.OpeningTable.OpeningTableSettings
import Chess.OpeningTable.PersistSqlOpeningTable
import Control.Monad.Logger
import Control.Monad.Reader
import Database.Persist.Postgresql (SqlPersistT)

newtype OpeningTableReader a =
  OpeningTableReader
    { getOpeningTableReader :: ReaderT OpeningTableSettings (LoggingT IO) a
    }
  deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadLoggerIO)

liftPersistSql :: PersistSqlOpeningTable a -> OpeningTableReader a
liftPersistSql action = OpeningTableReader output
  where
    output = do
      connString <- asks getConnectionString
      lift $ runAction connString action

instance OpeningTableAccessor OpeningTableReader where
  lookupFenInOpeningTable fen = liftPersistSql (lookupFenInOpeningTable fen)

instance OpeningTableInserter OpeningTableReader where
  migrateDb = liftPersistSql migrateDb
  insertFenWithEvaluation fen eval = liftPersistSql (insertFenWithEvaluation fen eval)
