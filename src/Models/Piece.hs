module Models.Piece (Piece, pieceColour, pieceType) where

import Data.Char (toLower, toUpper)
import Models.PieceColour
import Models.PieceType (PieceType)

data Piece = Piece {pieceColour :: PieceColour, pieceType :: PieceType} deriving (Eq)

instance Show Piece where
  show (Piece pieceColour pieceType) =
    if pieceColour == White
      then map toUpper (show pieceType)
      else map toLower (show pieceType)
