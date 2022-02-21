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

data Position = Position {_fen :: FenRepresentation, _pieceList :: FullPieceList}

makeLenses ''Position

getLikePieces :: Position -> PieceList
getLikePieces pos = case view (fen . nextToMove) pos of
  White -> view (pieceList . whitePieces) pos
  Black -> view (pieceList . blackPieces) pos

getLikeOccupiedSquares :: Position -> Squares
getLikeOccupiedSquares pos = case view (fen . nextToMove) pos of
  White -> view (pieceList . whiteOccupiedSquares) pos
  Black -> view (pieceList . blackOccupiedSquares) pos

getOppoOccupiedSquares :: Position -> Squares
getOppoOccupiedSquares pos = case view (fen . nextToMove) pos of
  White -> view (pieceList . blackOccupiedSquares) pos
  Black -> view (pieceList . whiteOccupiedSquares) pos

-- inCheck :: Position -> Bool
-- inCheck pos = case view (fen . nextToMove) pos of
--   White -> view (pieceList . whiteKing) pos `Set.member` getOppoAttackedSquares pos
--   Black -> view (pieceList . blackKing) pos `Set.member` getOppoAttackedSquares pos
