{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Chess.OpeningTable.PersistSqlOpeningTable where

import Chess.OpeningTable.OpeningTableAccessor
import Chess.OpeningTable.OpeningTableInserter
import Chess.OpeningTable.OpeningTablePersist
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Database.Persist as PS
import Database.Persist.Class as PS
import Database.Persist.Postgresql

newtype PersistSqlOpeningTable a =
  PersistSqlOpeningTable
    { unPersist :: SqlPersistT (LoggingT IO) a
    }
  deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadLoggerIO)

runAction :: ConnectionString -> PersistSqlOpeningTable a -> LoggingT IO a
runAction connString (PersistSqlOpeningTable action) =
  withPostgresqlConn connString $ \backend -> runSqlConn action backend

instance OpeningTableAccessor PersistSqlOpeningTable where
  lookupFenInOpeningTable fen = PersistSqlOpeningTable output
    where
      output = do
        _ <- $(logDebugSH) fen
        openingPos <- PS.get $ fenToOpeningPositionKey fen
        _ <- $(logDebugSH) openingPos
        return $ _openingPositionEvaluation <$> openingPos

instance OpeningTableInserter PersistSqlOpeningTable where
  migrateDb = PersistSqlOpeningTable (runMigration migrateAll)
  insertFenWithEvaluation fen eval = PersistSqlOpeningTable output
    where
      (key, openingPos) = fenWithEvalToOpeningPosition fen eval
      output = insertKey key openingPos
