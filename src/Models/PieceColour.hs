{-# LANGUAGE DeriveDataTypeable #-}

module Models.PieceColour (PieceColour (..)) where

import Data.Data
import Text.Read

data PieceColour = White | Black deriving (Eq, Enum, Data, Typeable)

instance Show PieceColour where
  show White = "w"
  show Black = "b"

instance Read PieceColour where
  readPrec =
    do
      Ident s <- lexP
      case s of
        "w" -> return White
        "b" -> return Black
        _ -> pfail

  readListPrec = readListPrecDefault
  readList = readListDefault
