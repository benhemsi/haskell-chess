module Models.FullPieceList (FullPieceList (..)) where

import Models.PieceColour
import Models.PieceList
import Models.Square

data FullPieceList = FullPieceList
  { whitePieces, blackPieces :: PieceList,
    whiteOccupiedSquares, blackOccupiedSquares :: Squares
  }

getPieceList :: PieceColour -> FullPieceList -> PieceList
getPieceList colour = case colour of 
                        White -> whitePieces
                        Black -> blackPieces
