module Models.GameEnd where

import Models.PieceColour

data GameEnd
  = Checkmate PieceColour
  | Stalemate
  | Repetition
  | InsufficientMaterial
