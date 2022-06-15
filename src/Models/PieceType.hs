{-# LANGUAGE ExistentialQuantification #-}

module Models.PieceType where

import Models.Move
import Piece.Bishop
import Piece.King
import Piece.Knight
import Piece.Pawn
import Piece.Queen
import Piece.Rook
import Test.QuickCheck
import Text.Read

data PieceType
  = King
  | Queen
  | Rook
  | Bishop
  | Knight
  | Pawn
  deriving (Eq, Enum, Bounded)

data PieceExi =
  forall p. (Moveable p, Show p, Read p) =>
            PieceExi p

evalPiece :: PieceType -> PieceExi
evalPiece King = PieceExi K
evalPiece Queen = PieceExi Q
evalPiece Rook = PieceExi R
evalPiece Bishop = PieceExi B
evalPiece Knight = PieceExi N
evalPiece Pawn = PieceExi P

instance Moveable PieceExi where
  emptyBoardMoves (PieceExi p) = emptyBoardMoves p

instance Moveable PieceType where
  emptyBoardMoves pieceType = emptyBoardMoves (evalPiece pieceType)

instance Show PieceExi where
  show (PieceExi p) = show p

instance Show PieceType where
  show pieceType = show (evalPiece pieceType)

instance Read PieceType where
  readPrec = do
    Ident s <- lexP
    case s of
      "K" -> return King
      "Q" -> return Queen
      "R" -> return Rook
      "B" -> return Bishop
      "N" -> return Knight
      "P" -> return Pawn
      _ -> pfail

instance Arbitrary PieceType where
  arbitrary = chooseEnum (minBound, maxBound)
