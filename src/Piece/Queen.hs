module Piece.Queen where

import Chess.Move
import Chess.Weighted
import Piece.Bishop
import Piece.Rook

data Queen =
  Q
  deriving (Show, Read)

instance Moveable Queen where
  emptyBoardMoves Q startSq = QueenMoves (getSlidingMoves bishopMvs) (getSlidingMoves rookMvs)
    where
      getSlidingMoves :: Moves -> SlidingMoves
      getSlidingMoves (Sliders moves) = moves
      getSlidingMoves _ = SlidingMoves [] [] [] []
      bishopMvs = emptyBoardMoves B startSq
      rookMvs = emptyBoardMoves R startSq

instance Weighted Queen where
  weight _ = 9
