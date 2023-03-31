{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Models.Position where

import Control.Lens
import Data.Map as Map
import Models.Board
import Models.Fen
import Models.Piece

data Position =
  Position
    { _fen :: FenRepresentation
    }
  deriving (Eq, Show)

makeLenses ''Position

buildBasePosition :: PieceList -> Position
buildBasePosition pl = Position (buildBaseFenRepresentation pl)

likePieces :: Lens' Position (Map.Map Square PieceType)
likePieces = likeLens (fen . pieces . whitePieces) (fen . pieces . blackPieces)

oppoPieces :: Lens' Position (Map.Map Square PieceType)
oppoPieces = oppoLens (fen . pieces . whitePieces) (fen . pieces . blackPieces)

likeOccupiedSquares :: Getter Position Squares
likeOccupiedSquares = likeGetter (fen . pieces . whiteOccupiedSquares) (fen . pieces . blackOccupiedSquares)

oppoOccupiedSquares :: Getter Position Squares
oppoOccupiedSquares = oppoGetter (fen . pieces . whiteOccupiedSquares) (fen . pieces . blackOccupiedSquares)

likeKingSquare :: Lens' Position Square
likeKingSquare = likeLens (fen . pieces . whiteKingSquare) (fen . pieces . blackKingSquare)

oppoKingSquare :: Lens' Position Square
oppoKingSquare = oppoLens (fen . pieces . whiteKingSquare) (fen . pieces . blackKingSquare)

kingSidePrivileges :: Lens' Position Bool
kingSidePrivileges = likeLens (fen . castlingPrivileges . whiteKingSide) (fen . castlingPrivileges . blackKingSide)

queenSidePrivileges :: Lens' Position Bool
queenSidePrivileges = likeLens (fen . castlingPrivileges . whiteQueenSide) (fen . castlingPrivileges . blackQueenSide)

likeLens :: Lens' Position a -> Lens' Position a -> Lens' Position a
likeLens whiteLens blackLens aToFa pos =
  let colour = view (fen . nextToMove) pos
      output =
        case colour of
          White -> (\x -> set whiteLens x pos) <$> aToFa (pos ^. whiteLens)
          Black -> (\x -> set blackLens x pos) <$> aToFa (pos ^. blackLens)
   in output

oppoLens :: Lens' Position a -> Lens' Position a -> Lens' Position a
oppoLens whiteLens blackLens = likeLens blackLens whiteLens

likeGetter :: Getter Position a -> Getter Position a -> Getter Position a
likeGetter whiteGetter blackGetter aToFa pos =
  let colour = view (fen . nextToMove) pos
      output =
        case colour of
          White -> pos <$ aToFa (pos ^. whiteGetter)
          Black -> pos <$ aToFa (pos ^. blackGetter)
   in output

oppoGetter :: Getter Position a -> Getter Position a -> Getter Position a
oppoGetter whiteGetter blackGetter = likeGetter blackGetter whiteGetter

switchNextToMove :: Position -> Position
switchNextToMove pos = outputPos
  where
    currentColour = view (fen . nextToMove) pos
    outputPos = set (fen . nextToMove) (oppoColour currentColour) pos
--inCheck :: Position -> Bool
--inCheck pos = case view (fen . nextToMove) pos of
--  White -> view (fen.pieces . whiteKing) pos `Set.member` getOppoAttackedSquares pos
--  Black -> view (fen.pieces . blackKing) pos `Set.member` getOppoAttackedSquares pos
