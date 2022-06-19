module Models.GameEnd where

import Models.Piece

data GameEnd
  = Checkmate PieceColour
  | Stalemate
  | Repetition
  | InsufficientMaterial
