module Piece.Piece where

import Data.Functor.Rep
import Models.Board
import Models.FullPieceList
import Models.Move
import Models.Piece
import Models.PieceColour
import Models.PieceOnSquare
import Models.PieceType
import Models.Square
import qualified Piece.Bishop as B
import qualified Piece.King as K
import qualified Piece.Knight as N
import qualified Piece.Pawn as P
import qualified Piece.Queen as Q
import qualified Piece.Rook as R

-- data PieceMoves = PieceMoves
--   { kingMoves :: K.KingMoves,
--     queenMoves :: Q.QueenMoves,
--     rookMoves :: R.RookMoves,
--     bishopMoves :: B.BishopMoves,
--     knightMoves :: N.KnightMoves,
--     pawnMoves :: P.PawnMoves
--   }

-- getEmptyBoardMoves :: Square -> PieceMoves
-- getEmptyBoardMoves start = PieceMoves (K.emptyBoardMoves start) (Q.emptyBoardMoves start) (R.emptyBoardMoves start) (B.emptyBoardMoves start) (N.emptyBoardMoves start) (P.emptyBoardMoves start)

-- moveBoard :: Board PieceMoves
-- moveBoard = tabulate getEmptyBoardMoves

-- emptyBoardMoves :: Square -> PieceMoves
-- emptyBoardMoves = index moveBoard

-- validMoves :: PieceOnSquare -> FullPieceList -> Moves
-- validMoves (PieceOnSquare (Piece colour pieceT) start) fullPL = pieceMoves
--   where
--     unfilteredMoves = emptyBoardMoves start

--     (likeOccupiedSquares, oppoOccupiedSquares) = case colour of
--       White -> (whiteOccupiedSquares fullPL, blackOccupiedSquares fullPL)
--       Black -> (blackOccupiedSquares fullPL, whiteOccupiedSquares fullPL)

--     pieceMoves = case pieceT of
--       King -> K.validMoves (kingMoves unfilteredMoves) likeOccupiedSquares
--       Queen -> Q.validMoves (queenMoves unfilteredMoves) likeOccupiedSquares oppoOccupiedSquares
--       Rook -> R.validMoves (rookMoves unfilteredMoves) likeOccupiedSquares oppoOccupiedSquares
--       Bishop -> B.validMoves (bishopMoves unfilteredMoves) likeOccupiedSquares oppoOccupiedSquares
--       Knight -> N.validMoves (knightMoves unfilteredMoves) likeOccupiedSquares
--       Pawn -> filteredPawnMoves
--         where
--           fullPawnMoves = P.validMoves (pawnMoves unfilteredMoves) (whiteOccupiedSquares fullPL) (blackOccupiedSquares fullPL)
--           filteredPawnMoves = case colour of
--             White -> P.white fullPawnMoves
--             Black -> P.black fullPawnMoves

-- pieceListMoves :: PieceColour -> FullPieceList -> Moves
-- pieceListMoves colour fullPL =
--   case colour of
--     White -> whitePieces fullPL >>= (`validMoves` fullPL)
--     Black -> blackPieces fullPL >>= (`validMoves` fullPL)