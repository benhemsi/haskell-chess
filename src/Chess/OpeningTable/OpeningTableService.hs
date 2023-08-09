{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chess.OpeningTable.OpeningTableService where

import Chess.OpeningTable.OpeningTableAccessor
import Chess.OpeningTable.OpeningTableInserter
import Chess.OpeningTable.OpeningTableSettings
import Chess.OpeningTable.PersistSqlOpeningTable
import Control.Monad.Logger
import Control.Monad.Reader
import Database.Persist.Postgresql (SqlPersistT)

newtype OpeningTableService a =
  OpeningTableService
    { getOpeningTableService :: ReaderT OpeningTableSettings (LoggingT IO) a
    }
  deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadLoggerIO)

instance OpeningTableAccessor OpeningTableService where
  lookupFenInOpeningTable fen = liftPersistSql (lookupFenInOpeningTable fen)

instance OpeningTableInserter OpeningTableService where
  migrateDb = liftPersistSql migrateDb
  insertFenWithEvaluation fen eval = liftPersistSql (insertFenWithEvaluation fen eval)

liftPersistSql :: PersistSqlOpeningTable a -> OpeningTableService a
liftPersistSql action = OpeningTableService output
  where
    output = do
      connString <- asks getConnectionString
      lift $ runAction connString action
