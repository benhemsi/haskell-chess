module Chess.OpeningTable.OpeningTableBuilder where

import Chess.OpeningTable.OpeningTableSettings

class OpeningTableBuilder m where
  buildOpeningTable :: FilePath -> m OpeningTableSettings
-- instance OpeningTableBuilder IO where
--   buildOpeningTable settingsConfig = output where
--     do settingsConfig <- readSettingsYaml
