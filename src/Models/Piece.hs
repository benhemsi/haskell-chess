{-# LANGUAGE TemplateHaskell #-}

module Models.Piece where

import Control.Lens hiding (elements)
import Data.Char (isUpper, toLower, toUpper)
import Models.Move
import Models.PieceColour
import Models.PieceType
import Test.QuickCheck
import Text.Read

data Piece =
  Piece
    { _pieceColour :: PieceColour
    , _pieceType :: PieceType
    }
  deriving (Eq)

makeLenses ''Piece

instance Show Piece where
  show (Piece pieceClr pieceTpe) =
    if pieceClr == White
      then map toUpper (show pieceTpe)
      else map toLower (show pieceTpe)

instance Read Piece where
  readPrec = do
    Ident s <- lexP
    let colour =
          if all isUpper s
            then White
            else Black
        piece = readMaybe (map toUpper s)
    case piece of
      Just p -> return (Piece colour p)
      Nothing -> pfail
  readListPrec = readListPrecDefault
  readList = readListDefault

instance Moveable Piece where
  emptyBoardMoves (Piece _ tpe) = emptyBoardMoves tpe

instance Arbitrary Piece where
  arbitrary = elements [Piece c t | c <- [White, Black], t <- [minBound .. maxBound]]

whitePawn :: Piece
whitePawn = Piece White Pawn

blackPawn :: Piece
blackPawn = Piece Black Pawn

whiteKnight :: Piece
whiteKnight = Piece White Knight

blackKnight :: Piece
blackKnight = Piece Black Knight

whiteBishop :: Piece
whiteBishop = Piece White Bishop

blackBishop :: Piece
blackBishop = Piece Black Bishop

whiteRook :: Piece
whiteRook = Piece White Rook

blackRook :: Piece
blackRook = Piece Black Rook

whiteQueen :: Piece
whiteQueen = Piece White Queen

blackQueen :: Piece
blackQueen = Piece Black Queen

whiteKing :: Piece
whiteKing = Piece White King

blackKing :: Piece
blackKing = Piece Black King
