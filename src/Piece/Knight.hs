module Piece.Knight where

import Models.Board
import Models.Move
import Models.Weighted

data Knight =
  N
  deriving (Show, Read)

instance Moveable Knight where
  emptyBoardMoves N startSq =
    let startFile = fromEnum (_file startSq)
        startRank = fromEnum (_rank startSq)
     in Moves
          [ Move startSq (Square (toEnum f) (toEnum r))
          | i <- [-2, -1, 1, 2]
          , j <- [-2, -1, 1, 2]
          , abs (i * j) == 2
          , let f = startFile + i
                r = startRank + j
          , f >= 0 && f <= 8
          , r >= 0 && r <= 8
          ]

instance Weighted Knight where
  weight _ = 3
