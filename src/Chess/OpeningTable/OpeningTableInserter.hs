module Chess.OpeningTable.OpeningTableInserter where

import Chess.Fen (FenRepresentation)
import Chess.OpeningTable.OpeningTablePersist
import Chess.OpeningTable.OpeningTableReader
import Chess.OpeningTable.PersistSqlOpeningTable
import Control.Monad.Logger
import Control.Monad.Reader
import Database.Persist.Class
import Database.Persist.Postgresql

class OpeningTableInserter m where
  migrateDb :: m ()
  insertFenWithEvaluation :: FenRepresentation -> Double -> m ()

instance OpeningTableInserter PersistSqlOpeningTable where
  migrateDb = PersistSqlOpeningTable (runMigration migrateAll)
  insertFenWithEvaluation fen eval = PersistSqlOpeningTable output
    where
      (key, openingPos) = fenWithEvalToOpeningPosition fen eval
      output = insertKey key openingPos

instance OpeningTableInserter OpeningTableReader where
  migrateDb = liftPersistSql migrateDb
  insertFenWithEvaluation fen eval = liftPersistSql (insertFenWithEvaluation fen eval)
