module Piece (Piece, PieceType(King, Queen, Rook, Bishop, Knight, Pawn), PieceColour(White, Black), pieceColour, pieceType) where
import Data.Char (toUpper, toLower)

data PieceColour = White | Black deriving Eq
data PieceType = King | Queen | Rook | Bishop | Knight | Pawn deriving Eq
data Piece = Piece { pieceColour :: PieceColour, pieceType :: PieceType } deriving Eq

instance Show PieceType where
  show King = "K"
  show Queen = "Q"
  show Rook = "R"
  show Bishop = "B"
  show Knight = "N" 
  show Pawn = "P"

instance Show Piece where
  show (Piece pieceColour pieceType) =
    if pieceColour == White
      then map toUpper (show pieceType)
      else map toLower (show pieceType)
