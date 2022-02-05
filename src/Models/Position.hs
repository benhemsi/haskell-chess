module Models.Position where

import Models.FenRepresentation
import Models.FullPieceList

data Postion = Postion {fen :: FenRepresentation, pieceList :: FullPieceList}
