module Models.PieceOnSquare (PieceOnSquare, piece, square) where
import Models.Piece
import Models.Square (Square)

data PieceOnSquare =
  PieceOnSquare
    { piece :: Piece,
      square :: Square
    } deriving Eq

