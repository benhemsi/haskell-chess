module Models.PieceColour (PieceColour(White, Black)) where
import Text.Read
import Text.ParserCombinators.ReadPrec 

data PieceColour = White | Black deriving Eq

instance Show PieceColour where
  show White = "w"
  show Black = "b"

readPieceColour :: Char -> PieceColour
readPieceColour 'w' = White
readPieceColour 'b' = Black
readPieceColour c = error (c : " is an invalid piece colour. Use w or b.")

instance Read PieceColour where
  readPrec =
    prec
      0
      ( do
          Char x <- lexP
          return (readPieceColour x)
      )
