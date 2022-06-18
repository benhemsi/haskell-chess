module Piece.Rook where

import Models.Move
import Models.Square
import Models.Weighted

data Rook =
  R
  deriving (Show, Read)

instance Moveable Rook where
  emptyBoardMoves R startSq =
    let startFile = _file startSq
        startRank = _rank startSq
        north =
          if startRank == R8
            then []
            else map (Move startSq . Square startFile) (getRange (succ startRank) R8)
        south =
          if startRank == R1
            then []
            else map (Move startSq . Square startFile) (getRange (pred startRank) R1)
        east =
          if startFile == Fh
            then []
            else map (Move startSq . flip Square startRank) (getRange (succ startFile) Fh)
        west =
          if startFile == Fa
            then []
            else map (Move startSq . flip Square startRank) (getRange (pred startFile) Fa)
     in Sliders $ SlidingMoves north east south west

instance Weighted Rook where
  weight _ = 5
