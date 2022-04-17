{-# LANGUAGE ParallelListComp #-}

module Piece.Bishop where

import Models.File
import Models.Move
import Models.Rank
import Models.Square
import Piece.Moveable

data Bishop =
  B
  deriving (Show, Read)

instance Moveable Bishop where
  emptyBoardMoves B start = Sliders $ SlidingMoves northEast southEast southWest northWest
    where
      getDiagonal start target =
        let startFile = _file start
            startRank = _rank start
            targetFile = _file target
            targetRank = _rank target
         in filter
              (Move start start /=)
              [Move start (Square f r) | f <- getRange startFile targetFile | r <- getRange startRank targetRank]
      northEast = getDiagonal start (Square Fh R8)
      southEast = getDiagonal start (Square Fh R1)
      southWest = getDiagonal start (Square Fa R1)
      northWest = getDiagonal start (Square Fa R8)
