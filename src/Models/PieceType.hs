module Models.PieceType (PieceType (..)) where

import Piece.King
import Piece.Queen
import Piece.Rook
import Piece.Bishop
import Piece.Knight
import Piece.Pawn
import Models.Move

data PieceType = King | Queen | Rook | Bishop | Knight | Pawn deriving (Eq, Enum, Bounded, Show, Read)

instance Moveable PieceType where
  emptyBoardMoves King sq = emptyBoardMoves K sq
  emptyBoardMoves Queen sq = emptyBoardMoves Q sq
  emptyBoardMoves Rook sq = emptyBoardMoves R sq
  emptyBoardMoves Bishop sq = emptyBoardMoves B sq
  emptyBoardMoves Knight sq = emptyBoardMoves N sq
  emptyBoardMoves Pawn sq = emptyBoardMoves P sq
