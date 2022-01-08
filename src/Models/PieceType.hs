{-# LANGUAGE DeriveDataTypeable #-}

module Models.PieceType (PieceType (..)) where

import Data.Data
import Text.Read

data PieceType = King | Queen | Rook | Bishop | Knight | Pawn deriving (Eq, Enum, Bounded, Data, Typeable)

instance Show PieceType where
  show King = "K"
  show Queen = "Q"
  show Rook = "R"
  show Bishop = "B"
  show Knight = "N"
  show Pawn = "P"

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

  readListPrec = readListPrecDefault
  readList = readListDefault
