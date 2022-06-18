{-# LANGUAGE TemplateHaskell #-}

module Models.Position where

import Control.Lens
import Models.CastlingPrivileges
import Models.FenRepresentation
import Models.FullPieceList
import Models.PieceColour
import Models.PieceList
import Models.Square

data Position =
  Position
    { _fen :: FenRepresentation
    , _pieceList :: FullPieceList
    }

makeLenses ''Position

buildBasePosition :: PieceList -> Position
buildBasePosition pl = Position (buildBaseFenRepresentation pl) (buildBaseFullPieceList pl)

getLikePieces :: Position -> PieceList
getLikePieces pos =
  case view (fen . nextToMove) pos of
    White -> view (pieceList . whitePieces) pos
    Black -> view (pieceList . blackPieces) pos

getOppoPieces :: Position -> PieceList
getOppoPieces pos =
  case view (fen . nextToMove) pos of
    White -> view (pieceList . blackPieces) pos
    Black -> view (pieceList . whitePieces) pos

getLikeOccupiedSquares :: Position -> Squares
getLikeOccupiedSquares pos =
  case view (fen . nextToMove) pos of
    White -> view (pieceList . whiteOccupiedSquares) pos
    Black -> view (pieceList . blackOccupiedSquares) pos

getOppoOccupiedSquares :: Position -> Squares
getOppoOccupiedSquares pos =
  case view (fen . nextToMove) pos of
    White -> view (pieceList . blackOccupiedSquares) pos
    Black -> view (pieceList . whiteOccupiedSquares) pos

getKingSidePrivileges :: Position -> Bool
getKingSidePrivileges pos =
  case view (fen . nextToMove) pos of
    White -> view (fen . castlingPrivileges . whiteKingSide) pos
    Black -> view (fen . castlingPrivileges . blackKingSide) pos

getQueenSidePrivileges :: Position -> Bool
getQueenSidePrivileges pos =
  case view (fen . nextToMove) pos of
    White -> view (fen . castlingPrivileges . whiteQueenSide) pos
    Black -> view (fen . castlingPrivileges . blackQueenSide) pos

switchNextToMove :: Position -> Position
switchNextToMove pos = outputPos
  where
    currentColour = view (fen . nextToMove) pos
    outputPos = set (fen . nextToMove) (oppoColour currentColour) pos
--inCheck :: Position -> Bool
--inCheck pos = case view (fen . nextToMove) pos of
--  White -> view (pieceList . whiteKing) pos `Set.member` getOppoAttackedSquares pos
--  Black -> view (pieceList . blackKing) pos `Set.member` getOppoAttackedSquares pos
