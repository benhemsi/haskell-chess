module Move.QueenMoves where

import Models.Move
import Models.Square
import Move.BishopMoves as B
import Move.RookMoves as R

data QueenMoves = QueenMoves {rook :: RookMoves, bishop :: BishopMoves} deriving (Show)

emptyBoardMoves :: Square -> QueenMoves
emptyBoardMoves start = QueenMoves (R.emptyBoardMoves start) (B.emptyBoardMoves start)

flattenMoves :: QueenMoves -> Moves
flattenMoves queenMoves = R.flattenMoves (rook queenMoves) ++ B.flattenMoves (bishop queenMoves)
