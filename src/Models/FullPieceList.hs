module Models.FullPieceList (FullPieceList (..), OccupiedSquares (..)) where

import Models.PieceColour
import Models.PieceList
import Models.Square

data FullPieceList = forall p. FullPieceList
  { whitePieces, blackPieces :: PieceList,
    whiteOccupiedSquares, blackOccupiedSquares, whiteAttackedSquares, blackAttackedSquares :: Squares
  }

getPieceList :: PieceColour -> FullPieceList -> PieceList
getPieceList colour = case colour of
  White -> whitePieces
  Black -> blackPieces

data OccupiedSquares = OccupiedSquares {like, oppo :: Squares}
