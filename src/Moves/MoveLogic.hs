module Moves.MoveLogic where

import qualified Data.Set as Set
import Models.Move
import Models.FullPieceList
import Models.Square

filterSlidingMoves :: SlidingMoves -> OccupiedSquares -> [Move]
filterSlidingMoves (SlidingMoves a b c d) occupiedSquares = foldMoves a ++ foldMoves b ++ foldMoves c ++ foldMoves d
  where
    likePieces = like occupiedSquares
    oppoPieces = oppo occupiedSquares
    filterMoves :: [Move] -> [Move] -> [Move]
    filterMoves [] curr = curr
    filterMoves (move : moves) curr
      | end move `Set.member` likePieces = curr
      | end move `Set.member` oppoPieces = move : curr
      | otherwise = filterMoves moves (move : curr)
    foldMoves :: [Move] -> [Move]
    foldMoves moves = filterMoves moves []

-- Takes a list of moves and filters out any which end on squares occupied by like pieces
filterMoves :: [Move] -> OccupiedSquares -> [Move]
filterMoves emptyBoardMoves occupiedSquares = filterMoves emptyBoardMoves
  where
    likePieces = like occupiedSquares
    filterFunction :: Move -> Bool
    filterFunction (Move _ end) = end `Set.member` likePieces
    filterMoves :: [Move] -> [Move]
    filterMoves moves = filter filterFunction emptyBoardMoves

filterPawnMoves :: PawnMoves -> OccupiedSquares -> [Move]
filterPawnMoves (PM f1 f2 tl tr enPL enPR pr) occupiedSquares = forward ++ takes ++ enPassent ++ promotion
  where
    likePieces = like occupiedSquares
    oppoPieces = oppo occupiedSquares
    allPieces = likePieces `Set.union` oppoPieces

    filterMove :: Maybe Move -> [Move]
    filterMove Nothing = []
    filterMove (Just move) = [move | end move `Set.notMember` allPieces]

    forward1 = filterMove f1
    forward = case (forward1, f2) of
      ([], _) -> forward1
      _ -> forward1 ++ filterMove f2

    filterTake :: Maybe Move -> [Move]
    filterTake take = case take of
      Just move -> [move | end move `Set.member` oppoPieces]
      Nothing -> []
    takes = filterTake tl ++ filterTake tr

    enPassent = [] -- TODO fill this in when have sorted FEN rep
    promotion = filterMove (fmap (\(PawnPromotion move) -> move) pr)

filterKingMoves :: KingMoves -> OccupiedSquares -> Squares -> [Move]
filterKingMoves (KM moves kingSide queenSide) occupiedSquares attackedSquares = filterMoves moves occupiedSquares -- TODO add castling filtering
  -- where
  --   squaresToCheck

filterAllMoves :: Moves -> OccupiedSquares -> Squares -> [Move]
filterAllMoves (Moves moves) occupiedSquares _ = filterMoves moves occupiedSquares
filterAllMoves (Sliders moves) occupiedSquares _ = filterSlidingMoves moves occupiedSquares
filterAllMoves (QueenMoves bishopMoves rookMoves) occupiedSquares _ = filterSlidingMoves bishopMoves occupiedSquares ++ filterSlidingMoves rookMoves occupiedSquares
filterAllMoves (PawnMoves whiteMoves blackMoves) occupiedSquares _ = filterPawnMoves whiteMoves occupiedSquares ++ filterPawnMoves blackMoves occupiedSquares
filterAllMoves (KingMoves kingMoves) occupiedSquares attackedSquares = filterKingMoves kingMoves occupiedSquares attackedSquares

-- filterSlidingPieceMoves :: Data a => a -> Squares -> Squares -> a
-- filterSlidingPieceMoves moves likeOccupiedSquares oppoOccupiedSquares =
--   gmapT
--     ( \moves -> case cast moves of
--         Just mvs -> fromJust (cast (filterSlidingMoves mvs likeOccupiedSquares oppoOccupiedSquares))
--         Nothing -> moves
--     )
--     moves
