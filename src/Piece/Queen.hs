module Piece.Queen where

import Models.Move
import Piece.Bishop
import Piece.Moveable
import Piece.Rook

data Queen =
  Q
  deriving (Show, Read)

instance Moveable Queen where
  emptyBoardMoves Q start = QueenMoves (getSlidingMoves bishopMoves) (getSlidingMoves rookMoves)
    where
      getSlidingMoves :: Moves -> SlidingMoves
      getSlidingMoves (Sliders moves) = moves
      getSlidingMoves _ = SlidingMoves [] [] [] []
      bishopMoves = emptyBoardMoves B start
      rookMoves = emptyBoardMoves R start
