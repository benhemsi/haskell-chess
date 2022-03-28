{-# LANGUAGE TemplateHaskell #-}

module Models.FullPieceList where

import Control.Lens
import qualified Data.Set as Set
import Models.Piece
import Models.PieceOnSquare
import Models.PieceColour
import Models.PieceList
import Models.Square
import Models.File
import Models.Rank

data FullPieceList = FullPieceList
  { _whitePieces, _blackPieces :: PieceList,
    _whiteOccupiedSquares, _blackOccupiedSquares :: Squares,
    _whiteKing, _blackKing :: Square
  }

makeLenses ''FullPieceList

buildBaseFullPieceList :: PieceList -> FullPieceList
buildBaseFullPieceList pl = FullPieceList whitePieces blackPieces whiteSquares blackSquares (Square Fa R1) (Square Fa R1)
  where
    filterByColour colour = filter (\p -> view (piece . pieceColour) p == colour)
    whitePieces = filterByColour White pl
    blackPieces = filterByColour Black pl
    whiteSquares = Set.fromList $ map (view square) whitePieces
    blackSquares = Set.fromList $ map (view square) blackPieces

getPieceList :: PieceColour -> FullPieceList -> PieceList
getPieceList colour = case colour of
  White -> _whitePieces
  Black -> _blackPieces
