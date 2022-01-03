module Move.PieceMoves where

import Models.Square
import Models.Board
import Move.KingMoves as K
import Move.QueenMoves as Q
import Move.RookMoves as R
import Move.BishopMoves as B
import Move.KnightMoves as N
import Move.PawnMoves as P
import Data.Functor.Rep

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
