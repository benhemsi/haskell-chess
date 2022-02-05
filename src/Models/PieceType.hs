{-# LANGUAGE GADTs #-}

module Models.PieceType (PieceType (..)) where

import Piece.King
import Piece.Queen
import Piece.Rook
import Piece.Bishop
import Piece.Knight
import Piece.Pawn

data PieceType p where
  King :: p -> PieceType King
  Queen :: p -> PieceType Queen
  Rook :: p -> PieceType Rook
  Bishop :: p -> PieceType Bishop
  Knight :: p -> PieceType Knight
  Pawn:: p -> PieceType Pawn
