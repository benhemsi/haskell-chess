module Chess.OpeningTable.OpeningTableAccessor where

import Chess.Fen (FenRepresentation)
import Chess.OpeningTable.OpeningTablePersist
import Chess.OpeningTable.OpeningTableReader
import Chess.OpeningTable.PersistSqlOpeningTable
import Control.Monad.Reader
import Database.Persist.Class as PS
import Database.Persist.Postgresql

class OpeningTableAccessor m where
  lookupFenInOpeningTable :: FenRepresentation -> m (Maybe Double)

instance OpeningTableAccessor PersistSqlOpeningTable where
  lookupFenInOpeningTable fen = PersistSqlOpeningTable output
    where
      output = do
        openingPos <- PS.get $ fenToOpeningPositionKey fen
        return $ _openingPositionEvaluation <$> openingPos

instance OpeningTableAccessor OpeningTableReader where
  lookupFenInOpeningTable fen = liftPersistSql (lookupFenInOpeningTable fen)
