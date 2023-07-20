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

runAction :: ConnectionString -> PersistSqlOpeningTable a -> IO a
runAction connString (PersistSqlOpeningTable action) =
  runStdoutLoggingT $ withPostgresqlConn connString $ \backend -> runSqlConn action backend

instance OpeningTableAccessor PersistSqlOpeningTable where
  lookupFenInOpeningTable fen = PersistSqlOpeningTable output
    where
      output = do
        openingPos <- PS.get $ fenToOpeningPositionKey fen
        return $ _openingPositionEvaluation <$> openingPos

instance OpeningTableInserter PersistSqlOpeningTable where
  migrateDb = PersistSqlOpeningTable (runMigration migrateAll)
  insertFenWithEvaluation fen eval = PersistSqlOpeningTable output
    where
      (key, openingPos) = fenWithEvalToOpeningPosition fen eval
      output = insertKey key openingPos
