module Chess.OpeningTable.OpeningTableBuilder where

import Chess.Fen (startingFenRepresentation)
import Chess.OpeningTable.OpeningTableInserter
import Chess.OpeningTable.OpeningTablePersist
import Chess.OpeningTable.OpeningTableReader
import Chess.OpeningTable.OpeningTableSettings
import Control.Monad.Except
import Control.Monad.Reader (ReaderT(runReaderT))
import qualified Data.Yaml as Y

class OpeningTableBuilder m where
  buildOpeningTable :: FilePath -> m OpeningTableSettings

instance OpeningTableBuilder IO where
  buildOpeningTable settingsConfigPath = settings
    where
      settings = do
        settingsConfig <- Y.decodeFileEither settingsConfigPath
        settings <-
          case settingsConfig of
            Left error -> throwError (userError $ Y.prettyPrintParseException error)
            Right value -> return value
        let sqlToRun = do
              _ <- migrateDb
              insertFenWithEvaluation startingFenRepresentation 0.0
        _ <- runReaderT (unRead sqlToRun) settings
        return settings
