module Chess.GameEnd where

import Chess.Piece

data GameEnd
  = Checkmate PieceColour
  | Stalemate
  | Repetition
  | InsufficientMaterial
