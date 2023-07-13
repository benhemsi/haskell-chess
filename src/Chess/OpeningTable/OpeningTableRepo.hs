module Chess.OpeningTable.OpeningTableRepo where

import Chess.Fen (FenRepresentation)
import Chess.OpeningTable.OpeningTable
import Chess.OpeningTable.OpeningTablePostgres (connString, runAction)
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Map as Map
import Database.Persist.Class as PS
import Database.Persist.Postgresql

class OpeningTableRepo m where
  lookupFen :: FenRepresentation -> m (Maybe Double)

instance OpeningTableRepo MockOpeningTable where
  lookupFen fen = MockOpeningTable (asks (Map.lookup fen))

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
    { unMock :: Reader (Map.Map FenRepresentation Double) a
    }

newtype PersistSqlOpeningTable a =
  PersistSqlOpeningTable
    { unPersist :: SqlPersistT (LoggingT IO) a
    }
