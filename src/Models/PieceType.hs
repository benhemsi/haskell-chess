module Models.PieceType (PieceType (..)) where

import Models.Move
import Piece.Bishop
import Piece.King
import Piece.Knight
import Piece.Pawn
import Piece.Queen
import Piece.Rook
import Text.Read

data PieceType = King | Queen | Rook | Bishop | Knight | Pawn deriving (Eq, Enum, Bounded)

instance Show PieceType where
  show King = show K
  show Queen = show Q
  show Rook = show R
  show Bishop = show B 
  show Knight = show N
  show Pawn = show P

instance Read PieceType where
  readPrec =
    do
      Ident s <- lexP
      case s of
        "K" -> return King
        "Q" -> return Queen
        "R" -> return Rook
        "B" -> return Bishop
        "N" -> return Knight
        "P" -> return Pawn
        _ -> pfail
 
instance Moveable PieceType where
  emptyBoardMoves King = emptyBoardMoves K
  emptyBoardMoves Queen = emptyBoardMoves Q
  emptyBoardMoves Rook = emptyBoardMoves R
  emptyBoardMoves Bishop = emptyBoardMoves B
  emptyBoardMoves Knight = emptyBoardMoves N
  emptyBoardMoves Pawn = emptyBoardMoves P
