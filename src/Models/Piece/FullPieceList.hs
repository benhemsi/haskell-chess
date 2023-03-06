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
import Models.Piece.PieceType

data FullPieceList =
  FullPieceList
    { _whitePieces, _blackPieces :: PieceList
    , _whiteKingSquare, _blackKingSquare :: Square
    }
  deriving (Eq, Show)

makeLenses ''FullPieceList

buildFullPieceList :: PieceList -> FullPieceList
buildFullPieceList pl = FullPieceList whitePcs blackPcs (whiteKing ^. _1) (blackKing ^. _1)
  where
    filterByColour colour = Map.filter (\p -> view pieceColour p == colour)
    whitePcs = filterByColour White pl
    blackPcs = filterByColour Black pl
    whiteKing = head $ filter (\p -> p ^. _2 . pieceType == King) (Map.assocs whitePcs)
    blackKing = head $ filter (\p -> p ^. _2 . pieceType == King) (Map.assocs blackPcs)

whiteOccupiedSquares :: Getter FullPieceList Squares
whiteOccupiedSquares = whitePieces . occupiedSquares

blackOccupiedSquares :: Getter FullPieceList Squares
blackOccupiedSquares = blackPieces . occupiedSquares
