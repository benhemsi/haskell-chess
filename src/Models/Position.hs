{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Models.Position where

import Control.Lens
import Models.Board
import Models.Fen
import Models.Piece

data Position =
  Position
    { _fen :: FenRepresentation
    , _pieceList :: FullPieceList
    }
  deriving (Eq, Show)

makeLenses ''Position

buildBasePosition :: PieceList -> Position
buildBasePosition pl = Position (buildBaseFenRepresentation pl) (buildFullPieceList pl)

likePieces :: Lens' Position PieceList
likePieces = likeLens (pieceList . whitePieces) (pieceList . blackPieces)

oppoPieces :: Lens' Position PieceList
oppoPieces = oppoLens (pieceList . whitePieces) (pieceList . blackPieces)

likeOccupiedSquares :: Getter Position Squares
likeOccupiedSquares = likeGetter (pieceList . whiteOccupiedSquares) (pieceList . blackOccupiedSquares)

oppoOccupiedSquares :: Getter Position Squares
oppoOccupiedSquares = oppoGetter (pieceList . whiteOccupiedSquares) (pieceList . blackOccupiedSquares)

likeKingSquare :: Lens' Position Square
likeKingSquare = likeLens (pieceList . whiteKingSquare) (pieceList . blackKingSquare)

oppoKingSquare :: Lens' Position Square
oppoKingSquare = oppoLens (pieceList . whiteKingSquare) (pieceList . blackKingSquare)

kingSidePrivileges :: Lens' Position Bool
kingSidePrivileges = likeLens (fen . castlingPrivileges . whiteKingSide) (fen . castlingPrivileges . blackKingSide)

queenSidePrivileges :: Lens' Position Bool
queenSidePrivileges = likeLens (fen . castlingPrivileges . whiteQueenSide) (fen . castlingPrivileges . blackQueenSide)

likeLens :: Lens' Position a -> Lens' Position a -> Lens' Position a
likeLens whiteLens blackLens aToFa pos =
  let colour = view (fen . nextToMove) pos
      output =
        case colour of
          White -> (\x -> set whiteLens x pos) <$> aToFa (view whiteLens pos)
          Black -> (\x -> set blackLens x pos) <$> aToFa (view blackLens pos)
   in output

oppoLens :: Lens' Position a -> Lens' Position a -> Lens' Position a
oppoLens whiteLens blackLens = likeLens blackLens whiteLens

likeGetter :: Getter Position a -> Getter Position a -> Getter Position a
likeGetter whiteGetter blackGetter aToFa pos =
  let colour = view (fen . nextToMove) pos
      output =
        case colour of
          White -> pos <$ aToFa (view whiteGetter pos)
          Black -> pos <$ aToFa (view blackGetter pos)
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
--  White -> view (pieceList . whiteKing) pos `Set.member` getOppoAttackedSquares pos
--  Black -> view (pieceList . blackKing) pos `Set.member` getOppoAttackedSquares pos
