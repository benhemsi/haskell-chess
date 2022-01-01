module Models.Piece (Piece, pieceColour, pieceType) where
import Models.PieceType (PieceType)
import Models.PieceColour
import Data.Char (toUpper, toLower)

data Piece = Piece { pieceColour :: PieceColour, pieceType :: PieceType } deriving Eq

instance Show Piece where
  show (Piece pieceColour pieceType) =
    if pieceColour == White
      then map toUpper (show pieceType)
      else map toLower (show pieceType)
