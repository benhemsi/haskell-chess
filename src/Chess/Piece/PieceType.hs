{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}

module Chess.Piece.PieceType where

import Chess.Move
import Chess.Piece.Bishop
import Chess.Piece.King
import Chess.Piece.Knight
import Chess.Piece.Pawn
import Chess.Piece.Queen
import Chess.Piece.Rook
import Chess.Weighted
import Data.Aeson
import GHC.Generics
import Test.QuickCheck
import Text.Read

data PieceType
  = King
  | Queen
  | Rook
  | Bishop
  | Knight
  | Pawn
  deriving (Eq, Enum, Bounded, Generic)

data PieceExi =
  forall p. (Moveable p, Weighted p, Show p, Read p) =>
            PieceExi p

evalPiece :: PieceType -> PieceExi
evalPiece King = PieceExi K
evalPiece Queen = PieceExi Q
evalPiece Rook = PieceExi R
evalPiece Bishop = PieceExi B
evalPiece Knight = PieceExi N
evalPiece Pawn = PieceExi P

instance ToJSON PieceType

instance FromJSON PieceType

instance Moveable PieceExi where
  emptyBoardMoves (PieceExi p) = emptyBoardMoves p

instance Moveable PieceType where
  emptyBoardMoves pieceType = emptyBoardMoves (evalPiece pieceType)

instance Weighted PieceExi where
  weight (PieceExi p) = weight p

instance Weighted PieceType where
  weight pieceType = weight (evalPiece pieceType)

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
