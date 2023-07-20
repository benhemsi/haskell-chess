module Chess.OpeningTable.OpeningTableInserter where

import Chess.Fen (FenRepresentation)

class OpeningTableInserter m where
  migrateDb :: m ()
  insertFenWithEvaluation :: FenRepresentation -> Double -> m ()
