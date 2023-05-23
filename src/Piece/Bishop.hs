{-# LANGUAGE ParallelListComp #-}

module Piece.Bishop where

import Chess.Board
import Chess.Move
import Chess.Weighted

data Bishop =
  B
  deriving (Show, Read)

instance Moveable Bishop where
  emptyBoardMoves B startSq = Sliders $ SlidingMoves northEast southEast southWest northWest
    where
      getDiagonal sq target =
        let startFile = _file sq
            startRank = _rank sq
            targetFile = _file target
            targetRank = _rank target
         in filter
              (Move sq sq /=)
              [Move sq (Square f r) | f <- getRange startFile targetFile | r <- getRange startRank targetRank]
      northEast = getDiagonal startSq (Square Fh R8)
      southEast = getDiagonal startSq (Square Fh R1)
      southWest = getDiagonal startSq (Square Fa R1)
      northWest = getDiagonal startSq (Square Fa R8)

instance Weighted Bishop where
  weight _ = 3
