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
    } deriving (Eq, Show)

makeLenses ''Position

buildBasePosition :: PieceList -> Position
buildBasePosition pl = Position (buildBaseFenRepresentation pl) (buildBaseFullPieceList pl)

likePieces :: Lens' Position PieceList
likePieces = likeLens (pieceList . whitePieces) (pieceList . blackPieces)

oppoPieces :: Lens' Position PieceList
oppoPieces = oppoLens (pieceList . whitePieces) (pieceList . blackPieces)

likeOccupiedSquares :: Lens' Position Squares
likeOccupiedSquares = likeLens (pieceList . whiteOccupiedSquares) (pieceList . blackOccupiedSquares)

oppoOccupiedSquares :: Lens' Position Squares
oppoOccupiedSquares = oppoLens (pieceList . whiteOccupiedSquares) (pieceList . blackOccupiedSquares)

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
      output = case colour of 
        White -> (\x -> set whiteLens x pos) <$> aToFa (view whiteLens pos)
        Black -> (\x -> set blackLens x pos) <$> aToFa (view blackLens pos)
  in output
 
oppoLens :: Lens' Position a -> Lens' Position a -> Lens' Position a
oppoLens whiteLens blackLens = likeLens blackLens whiteLens
  
switchNextToMove :: Position -> Position
switchNextToMove pos = outputPos
  where
    currentColour = view (fen . nextToMove) pos
    outputPos = set (fen . nextToMove) (oppoColour currentColour) pos
    
--inCheck :: Position -> Bool
--inCheck pos = case view (fen . nextToMove) pos of
--  White -> view (pieceList . whiteKing) pos `Set.member` getOppoAttackedSquares pos
--  Black -> view (pieceList . blackKing) pos `Set.member` getOppoAttackedSquares pos
