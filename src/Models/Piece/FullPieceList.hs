{-# LANGUAGE TemplateHaskell #-}

module Models.Piece.FullPieceList where

import Control.Lens
import qualified Data.IntMap as Set
import qualified Data.Map as Map
import qualified Data.Set as Set
import Models.Board
import Models.Piece.Piece
import Models.Piece.PieceColour
import Models.Piece.PieceList
import Models.Piece.PieceOnSquare

data FullPieceList =
  FullPieceList
    { _whitePieces, _blackPieces :: PieceList
    , _whiteKingSquare, _blackKingSquare :: Square
    }
  deriving (Eq, Show)

makeLenses ''FullPieceList

buildBaseFullPieceList :: PieceList -> FullPieceList
buildBaseFullPieceList pl = FullPieceList whitePcs blackPcs (Square Fa R1) (Square Fa R1)
  where
    filterByColour colour = Map.filter (\p -> view pieceColour p == colour)
    whitePcs = filterByColour White pl
    blackPcs = filterByColour Black pl

whiteOccupiedSquares :: Getter FullPieceList Squares
whiteOccupiedSquares = whitePieces . occupiedSquares

blackOccupiedSquares :: Getter FullPieceList Squares
blackOccupiedSquares = blackPieces . occupiedSquares
