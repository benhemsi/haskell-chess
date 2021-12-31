module PieceList where
import Data.Set (Set, fromList)
import Piece
import Square (Square)

data PieceOnSquare =
  PieceOnSquare
    { piece :: Piece,
      square :: Square
    }

type PieceList = [PieceOnSquare]

type SquareSet = Set Square

data FullPieceList = FullPieceList
  { whitePieces, blackPieces :: PieceList,
    whiteOccupiedSquares, blackOccupiedSquares, whiteAttackedSquares, blackAttackedSquares :: SquareSet
  }

convertPieceList :: PieceList -> FullPieceList
convertPieceList pieceList = FullPieceList whitePieces blackPieces whiteOccupiedSquares blackOccupiedSquares whiteAttackedSquares blackAttackedSquares where
  whitePieces = filter (\p -> (pieceColour . piece) p == White) pieceList
  blackPieces = filter (\p -> (pieceColour . piece) p == Black) pieceList
  whiteOccupiedSquares = fromList (map square whitePieces)
  blackOccupiedSquares = fromList (map square whitePieces)
  whiteAttackedSquares = fromList (map square whitePieces)
  blackAttackedSquares = fromList (map square whitePieces)
