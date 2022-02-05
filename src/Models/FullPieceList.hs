{-# LANGUAGE TemplateHaskell #-}

module Models.FullPieceList where

import Control.Lens
import Models.PieceColour
import Models.PieceList
import Models.Square

data FullPieceList = FullPieceList
  { _whitePieces, _blackPieces :: PieceList,
    _whiteOccupiedSquares, _blackOccupiedSquares, _whiteAttackedSquares, _blackAttackedSquares :: Squares,
    _whiteKing, _blackKing :: Square
  }

makeLenses ''FullPieceList

getPieceList :: PieceColour -> FullPieceList -> PieceList
getPieceList colour = case colour of
  White -> _whitePieces
  Black -> _blackPieces
