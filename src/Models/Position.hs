{-# LANGUAGE TemplateHaskell #-}

module Models.Position where

import Control.Lens
import Models.CastlingPrivileges
import Models.FenRepresentation
import Models.FullPieceList
import Models.PieceColour
import Models.Square

data Position = Position {_fen :: FenRepresentation, _pieceList :: FullPieceList}

makeLenses ''Position

nextToMoveLens :: Lens' Position PieceColour
nextToMoveLens = fen . nextToMove

castlingPrivilegesLens :: Lens' Position CastlingPrivileges
castlingPrivilegesLens = fen . castlingPrivileges

enPassentLens :: Lens' Position (Maybe Square)
enPassentLens = fen . enPassentSquare

whiteOccupiedSquaresLens :: Lens' Position Squares
whiteOccupiedSquaresLens = pieceList . whiteOccupiedSquares

blackOccupiedSquaresLens :: Lens' Position Squares
blackOccupiedSquaresLens = pieceList . blackOccupiedSquares

whiteAttackedSquaresLens :: Lens' Position Squares
whiteAttackedSquaresLens = pieceList . whiteAttackedSquares

blackAttackedSquaresLens :: Lens' Position Squares
blackAttackedSquaresLens = pieceList . blackAttackedSquares

getLikeOccupiedSquares :: Position -> Squares
getLikeOccupiedSquares pos = case view nextToMoveLens pos of
  White -> view whiteOccupiedSquaresLens pos
  Black -> view blackOccupiedSquaresLens pos

getOppoOccupiedSquares :: Position -> Squares
getOppoOccupiedSquares pos = case view nextToMoveLens pos of
  White -> view blackOccupiedSquaresLens pos
  Black -> view whiteOccupiedSquaresLens pos

getLikeAttackedSquares :: Position -> Squares
getLikeAttackedSquares pos = case view nextToMoveLens pos of
  White -> view whiteAttackedSquaresLens pos
  Black -> view blackAttackedSquaresLens pos

getOppoAttackedSquares :: Position -> Squares
getOppoAttackedSquares pos = case view nextToMoveLens pos of
  White -> view blackAttackedSquaresLens pos
  Black -> view whiteAttackedSquaresLens pos
