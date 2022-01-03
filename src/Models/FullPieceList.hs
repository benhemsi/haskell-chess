module Models.FullPieceList where

import Models.PieceList
import Models.Square

data FullPieceList = FullPieceList
  { whitePieces, blackPieces :: PieceList,
    whiteOccupiedSquares, blackOccupiedSquares :: Squares
  }
