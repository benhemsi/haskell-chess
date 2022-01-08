module Models.PieceColour (PieceColour (..)) where

import Text.Read

data PieceColour = White | Black deriving (Eq, Enum)

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
