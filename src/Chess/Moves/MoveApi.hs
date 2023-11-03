module Chess.Moves.MoveApi where

import Chess.Fen
import Chess.Move

class MoveApi m where
  calculateLikeMoves :: FenRepresentation -> m [MoveTypes]
  calculateLikeAndOppoMoves :: FenRepresentation -> m ([MoveTypes], [MoveTypes])
