module Move.PieceMoves where

import Data.Functor.Rep
import Models.Board
import Models.FullPieceList
import Models.Move
import Models.Piece
import Models.PieceColour
import Models.PieceOnSquare
import Models.PieceType
import Models.Square
import qualified Move.BishopMoves as B
import qualified Move.KingMoves as K
import qualified Move.KnightMoves as N
import qualified Move.PawnMoves as P
import qualified Move.QueenMoves as Q
import qualified Move.RookMoves as R

data PieceMoves = PieceMoves
  { kingMoves :: K.KingMoves,
    queenMoves :: Q.QueenMoves,
    rookMoves :: R.RookMoves,
    bishopMoves :: B.BishopMoves,
    knightMoves :: N.KnightMoves,
    pawnMoves :: P.PawnMoves
  }

getEmptyBoardMoves :: Square -> PieceMoves
getEmptyBoardMoves start = PieceMoves (K.emptyBoardMoves start) (Q.emptyBoardMoves start) (R.emptyBoardMoves start) (B.emptyBoardMoves start) (N.emptyBoardMoves start) (P.emptyBoardMoves start)

moveBoard :: Board PieceMoves
moveBoard = tabulate getEmptyBoardMoves

emptyBoardMoves :: Square -> PieceMoves
emptyBoardMoves = index moveBoard

validMoves :: PieceOnSquare -> Squares -> Squares -> Moves
validMoves (PieceOnSquare (Piece colour pieceT) start) whiteOccupiedSquares blackOccupiedSquares = pieceMoves
  where
    unfilteredMoves = emptyBoardMoves start

    (likeOccupiedSquares, oppoOccupiedSquares) = case colour of
      White -> (whiteOccupiedSquares, blackOccupiedSquares)
      Black -> (blackOccupiedSquares, whiteOccupiedSquares)

    pieceMoves = case pieceT of
      King -> K.validMoves (kingMoves unfilteredMoves) likeOccupiedSquares
      Queen -> Q.validMoves (queenMoves unfilteredMoves) likeOccupiedSquares oppoOccupiedSquares
      Rook -> R.validMoves (rookMoves unfilteredMoves) likeOccupiedSquares oppoOccupiedSquares
      Bishop -> B.validMoves (bishopMoves unfilteredMoves) likeOccupiedSquares oppoOccupiedSquares
      Knight -> N.validMoves (knightMoves unfilteredMoves) likeOccupiedSquares
      Pawn -> filteredPawnMoves
        where
          fullPawnMoves = P.validMoves (pawnMoves unfilteredMoves) whiteOccupiedSquares blackOccupiedSquares
          filteredPawnMoves = case colour of
            White -> P.white fullPawnMoves
            Black -> P.black fullPawnMoves

pieceListMoves :: PieceColour -> FullPieceList -> Moves
pieceListMoves colour (FullPieceList whitePieces blackPieces whiteOccupiedSquares blackOccupiedSquares) =
  case colour of 
    White -> whitePieces >>= (\pos -> validMoves pos whiteOccupiedSquares blackOccupiedSquares)
    Black -> blackPieces >>= (\pos -> validMoves pos whiteOccupiedSquares blackOccupiedSquares)
