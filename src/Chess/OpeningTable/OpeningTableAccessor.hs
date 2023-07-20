module Chess.OpeningTable.OpeningTableAccessor where

import Chess.Fen (FenRepresentation)

class OpeningTableAccessor m where
  lookupFenInOpeningTable :: FenRepresentation -> m (Maybe Double)
