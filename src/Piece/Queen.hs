module Piece.Queen where

import Piece.Bishop
import Piece.Moveable
import Piece.Rook
import Models.Move

data Queen = Q deriving (Show, Read)

instance Moveable Queen where
  emptyBoardMoves _ start = QueenMoves (getSlidingMoves bishopMoves) (getSlidingMoves rookMoves)
    where
      getSlidingMoves :: Moves -> SlidingMoves
      getSlidingMoves (Sliders moves) = moves
      getSlidingMoves _ = SlidingMoves [] [] [] []

      bishopMoves = emptyBoardMoves B start
      rookMoves = emptyBoardMoves R start
