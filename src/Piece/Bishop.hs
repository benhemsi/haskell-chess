module Piece.Bishop where

import Models.File
import Models.Move
import Models.Rank
import Models.Square
import Piece.Moveable

data Bishop = B deriving (Show, Read)

instance Moveable Bishop where
  emptyBoardMoves B start = Sliders $ SlidingMoves northEast southEast southWest northWest
    where
      getDiagonal start target =
        let startFile = file start
            startRank = rank start
            targetFile = file target
            targetRank = rank target
         in zipWith
              (curry (Move start . uncurry Square))
              (getRange startFile targetFile)
              (getRange startRank targetRank)
      startFile = file start
      startRank = rank start
      northEast = if (startFile /= Fh) && (startRank /= R8) then getDiagonal (Square (succ startFile) (succ startRank)) (Square Fh R8) else []
      southEast = if (startFile /= Fh) && (startRank /= R1) then getDiagonal (Square (succ startFile) (pred startRank)) (Square Fh R1) else []
      southWest = if (startFile /= Fa) && (startRank /= R1) then getDiagonal (Square (pred startFile) (pred startRank)) (Square Fa R1) else []
      northWest = if (startFile /= Fa) && (startRank /= R8) then getDiagonal (Square (pred startFile) (succ startRank)) (Square Fa R8) else []
