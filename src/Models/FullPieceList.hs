{-# LANGUAGE TemplateHaskell #-}

module Models.FullPieceList where

import Control.Lens
import qualified Data.Set as Set
import Models.File
import Models.Piece
import Models.PieceColour
import Models.PieceList
import Models.PieceOnSquare
import Models.Rank
import Models.Square

data FullPieceList =
  FullPieceList
    { _whitePieces, _blackPieces :: PieceList
    , _whiteOccupiedSquares, _blackOccupiedSquares :: Squares
    , _whiteKing, _blackKing :: Square
    }

makeLenses ''FullPieceList

buildBaseFullPieceList :: PieceList -> FullPieceList
buildBaseFullPieceList pl =
  FullPieceList whitePcs blackPcs whiteSquares blackSquares (Square Fa R1) (Square Fa R1)
  where
    filterByColour colour = filter (\p -> view (piece . pieceColour) p == colour)
    whitePcs = filterByColour White pl
    blackPcs = filterByColour Black pl
    whiteSquares = Set.fromList $ map (view square) whitePcs
    blackSquares = Set.fromList $ map (view square) blackPcs

getPieceList :: PieceColour -> FullPieceList -> PieceList
getPieceList colour =
  case colour of
    White -> _whitePieces
    Black -> _blackPieces
