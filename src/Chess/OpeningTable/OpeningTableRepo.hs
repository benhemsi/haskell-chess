module Chess.OpeningTable.OpeningTableRepo where

import Chess.Fen (FenRepresentation)
import Chess.OpeningTable.OpeningTable
import Chess.OpeningTable.OpeningTablePostgres (connString, runAction)
import Control.Monad.Logger
import Control.Monad.Reader
import Database.Persist.Class as PS
import Database.Persist.Postgresql

class OpeningTableRepo m where
  lookupFen :: FenRepresentation -> m (Maybe Double)

instance OpeningTableRepo MockOpeningTable where
  lookupFen fen = MockOpeningTable ask

instance OpeningTableRepo PersistSqlOpeningTable where
  lookupFen fen = PersistSqlOpeningTable output
    where
      output = do
        openingPos <- PS.get $ fenToOpeningPositionKey fen
        return $ _openingPositionEvaluation <$> openingPos

instance OpeningTableRepo IO where
  lookupFen fen = runAction connString output
    where
      PersistSqlOpeningTable output = lookupFen fen

newtype MockOpeningTable a =
  MockOpeningTable
    { unMock :: Reader (Maybe Double) a
    }

newtype PersistSqlOpeningTable a =
  PersistSqlOpeningTable
    { unPersist :: SqlPersistT (LoggingT IO) a
    }
