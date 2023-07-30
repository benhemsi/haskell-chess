module Chess.OpeningTable.OpeningTableBuilder where

import Chess.Fen (startingFenRepresentation)
import Chess.OpeningTable.OpeningTableInserter
import Chess.OpeningTable.OpeningTablePersist
import Chess.OpeningTable.OpeningTableReader
import Chess.OpeningTable.OpeningTableSettings
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Yaml as Y

buildOpeningTable :: FilePath -> IO OpeningTableSettings
buildOpeningTable settingsConfigPath = settings
  where
    settings = do
      settings <- Y.decodeFileThrow settingsConfigPath
      let sqlToRun = do
            _ <- migrateDb
            insertFenWithEvaluation startingFenRepresentation 0.0
      _ <- runReaderT (getOpeningTableReader sqlToRun) settings
      return settings
