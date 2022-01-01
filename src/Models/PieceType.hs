module Models.PieceType (PieceType(King, Queen, Rook, Bishop, Knight, Pawn)) where

data PieceType = King | Queen | Rook | Bishop | Knight | Pawn deriving Eq

instance Show PieceType where
  show King = "K"
  show Queen = "Q"
  show Rook = "R"
  show Bishop = "B"
  show Knight = "N" 
  show Pawn = "P"
