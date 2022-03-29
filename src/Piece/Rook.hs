module Piece.Rook where

import Models.File
import Models.Move
import Models.Rank
import Models.Square
import Piece.Moveable

data Rook =
  R
  deriving (Show, Read)

instance Moveable Rook where
  emptyBoardMoves R start =
    let startFile = _file start
        startRank = _rank start
        north =
          if startRank == R8
            then []
            else map
                   (Move start . Square startFile)
                   (getRange (succ startRank) R8)
        south =
          if startRank == R1
            then []
            else map
                   (Move start . Square startFile)
                   (getRange (pred startRank) R1)
        east =
          if startFile == Fh
            then []
            else map
                   (Move start . flip Square startRank)
                   (getRange (succ startFile) Fh)
        west =
          if startFile == Fa
            then []
            else map
                   (Move start . flip Square startRank)
                   (getRange (pred startFile) Fa)
     in Sliders $ SlidingMoves north east south west
