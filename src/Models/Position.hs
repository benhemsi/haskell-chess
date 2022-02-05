{-# LANGUAGE TemplateHaskell #-}

module Models.Position where

import Control.Lens
import qualified Data.Set as Set
import Models.CastlingPrivileges
import Models.FenRepresentation
import Models.FullPieceList
import Models.Move
import Models.PieceColour
import Models.PieceList
import Models.Square

data Position = Position {_fen :: FenRepresentation, _pieceList :: FullPieceList, _moves :: [MoveTypes]}

makeLenses ''Position

nextToMoveLens :: Lens' Position PieceColour
nextToMoveLens = fen . nextToMove

castlingPrivilegesLens :: Lens' Position CastlingPrivileges
castlingPrivilegesLens = fen . castlingPrivileges

enPassentLens :: Lens' Position (Maybe Square)
enPassentLens = fen . enPassentSquare

whitePiecesLens :: Lens' Position PieceList
whitePiecesLens = pieceList . whitePieces

blackPiecesLens :: Lens' Position PieceList
blackPiecesLens = pieceList . blackPieces

whiteKingLens :: Lens' Position Square
whiteKingLens = pieceList . whiteKing

blackKingLens :: Lens' Position Square
blackKingLens = pieceList . blackKing

whiteOccupiedSquaresLens :: Lens' Position Squares
whiteOccupiedSquaresLens = pieceList . whiteOccupiedSquares

blackOccupiedSquaresLens :: Lens' Position Squares
blackOccupiedSquaresLens = pieceList . blackOccupiedSquares

whiteAttackedSquaresLens :: Lens' Position Squares
whiteAttackedSquaresLens = pieceList . whiteAttackedSquares

blackAttackedSquaresLens :: Lens' Position Squares
blackAttackedSquaresLens = pieceList . blackAttackedSquares

getLikePieces :: Position -> PieceList
getLikePieces pos = case view nextToMoveLens pos of
  White -> view whitePiecesLens pos
  Black -> view blackPiecesLens pos

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

inCheck :: Position -> Bool
inCheck pos = case view nextToMoveLens pos of
  White -> view whiteKingLens pos `Set.member` getOppoAttackedSquares pos
  Black -> view blackKingLens pos `Set.member` getOppoAttackedSquares pos
