module Moves.MoveLogic where

import Control.Lens
import qualified Data.Set as Set
import Models.Move
import Models.FullPieceList
import Models.Square
import Models.Position

filterSlidingMoves :: SlidingMoves -> Position -> [Move]
filterSlidingMoves (SlidingMoves a b c d) pos = foldMoves a ++ foldMoves b ++ foldMoves c ++ foldMoves d
  where
    likePieces = getLikeOccupiedSquares pos
    oppoPieces = getOppoOccupiedSquares pos
    filterMoves :: [Move] -> [Move] -> [Move]
    filterMoves [] curr = curr
    filterMoves (move : moves) curr
      | end move `Set.member` likePieces = curr
      | end move `Set.member` oppoPieces = move : curr
      | otherwise = filterMoves moves (move : curr)
    foldMoves :: [Move] -> [Move]
    foldMoves moves = filterMoves moves []

-- Takes a list of moves and filters out any which end on squares occupied by like pieces
filterMoves :: [Move] -> Position -> [Move]
filterMoves emptyBoardMoves pos = filterMoves emptyBoardMoves
  where
    likePieces = getLikeOccupiedSquares pos
    filterFunction :: Move -> Bool
    filterFunction (Move _ end) = end `Set.member` likePieces
    filterFunction _ = False
    filterMoves :: [Move] -> [Move]
    filterMoves moves = filter filterFunction emptyBoardMoves

filterPawnMoves :: PawnMoves -> Position -> [Move]
filterPawnMoves (PM f1 f2 tks enPs pr) pos = forward ++ takes ++ enPassent ++ promotion
  where
    likePieces = getLikeOccupiedSquares pos
    oppoPieces = getOppoOccupiedSquares pos
    allPieces = likePieces `Set.union` oppoPieces

    filterMove :: Maybe Move -> [Move]
    filterMove Nothing = []
    filterMove (Just move) = [move | end move `Set.notMember` allPieces]

    forward1 = filterMove f1
    forward = case (forward1, f2) of
      ([], _) -> forward1
      _ -> forward1 ++ filterMove f2

    takes = filter (\move -> end move `Set.member` oppoPieces) tks

    enPassent = case view enPassentLens pos of
      Just sq -> map EnP $ filter (\(EnPassent move _) -> end move == sq) enPs
      Nothing -> []

    promotion = filterMove (fmap PP pr)

filterKingMoves :: KingMoves -> Position -> [Move]
filterKingMoves (KM moves kingSide queenSide) = filterMoves moves -- TODO add castling filtering
  -- where
  --   squaresToCheck

filterAllMoves :: Moves -> Position -> [Move]
filterAllMoves (Moves moves) pos = filterMoves moves pos
filterAllMoves (Sliders moves) pos = filterSlidingMoves moves pos
filterAllMoves (QueenMoves bishopMoves rookMoves) pos = filterSlidingMoves bishopMoves pos ++ filterSlidingMoves rookMoves pos
filterAllMoves (PawnMoves whiteMoves blackMoves) pos = filterPawnMoves whiteMoves pos ++ filterPawnMoves blackMoves pos
filterAllMoves (KingMoves kingMoves) pos = filterKingMoves kingMoves pos

-- filterSlidingPieceMoves :: Data a => a -> Squares -> Squares -> a
-- filterSlidingPieceMoves moves likeOccupiedSquares oppoOccupiedSquares =
--   gmapT
--     ( \moves -> case cast moves of
--         Just mvs -> fromJust (cast (filterSlidingMoves mvs likeOccupiedSquares oppoOccupiedSquares))
--         Nothing -> moves
--     )
--     moves
