module Moves.MoveLogic where

import Control.Lens
import qualified Data.Set as Set
import Models.FullPieceList
import Models.Move
import Models.PieceOnSquare
import Models.PieceType
import Models.Position
import Models.Square

movesToMoveTypes :: [Move] -> [MoveTypes]
movesToMoveTypes = map Mv

filterSlidingMoves :: SlidingMoves -> Position -> [MoveTypes]
filterSlidingMoves (SlidingMoves a b c d) pos = movesToMoveTypes $ foldMoves a ++ foldMoves b ++ foldMoves c ++ foldMoves d
  where
    likePieces = getLikeOccupiedSquares pos
    oppoPieces = getOppoOccupiedSquares pos
    filterMoves :: [Move] -> [Move] -> [Move]
    filterMoves [] curr = curr
    filterMoves (move : moves) curr
      | _end move `Set.member` likePieces = curr
      | _end move `Set.member` oppoPieces = move : curr
      | otherwise = filterMoves moves (move : curr)
    foldMoves :: [Move] -> [Move]
    foldMoves moves = filterMoves moves []

filterMoves :: [Move] -> Position -> [MoveTypes]
filterMoves emptyBoardMoves pos = filterMoves emptyBoardMoves
  where
    likePieces = getLikeOccupiedSquares pos
    filterFunction :: Move -> Bool
    filterFunction (Move _ end) = end `Set.member` likePieces
    filterMoves :: [Move] -> [MoveTypes]
    filterMoves moves = movesToMoveTypes $ filter filterFunction emptyBoardMoves

filterPawnMoves :: PawnMoves -> Position -> [MoveTypes]
filterPawnMoves (PM f1 f2 tks enPs pr) pos = movesToMoveTypes (forward ++ takes) ++ enPassent ++ promotion
  where
    likePieces = getLikeOccupiedSquares pos
    oppoPieces = getOppoOccupiedSquares pos
    allPieces = likePieces `Set.union` oppoPieces

    filterMove :: Maybe Move -> [Move]
    filterMove Nothing = []
    filterMove (Just move) = [move | _end move `Set.notMember` allPieces]

    forward1 = filterMove f1
    forward = case (forward1, f2) of
      ([], _) -> forward1
      _ -> forward1 ++ filterMove f2

    takes = filter (\move -> _end move `Set.member` oppoPieces) tks

    enPassent = case view enPassentLens pos of
      Just sq -> map EnP $ filter (\(EnPassent move _) -> _end move == sq) enPs
      Nothing -> []

    promotion = map (PP . PawnPromotion) (filterMove $ fmap (\(PawnPromotion move) -> move) pr)

filterKingMoves :: KingMoves -> Position -> [MoveTypes]
filterKingMoves (KM moves kingSide queenSide) = filterMoves moves -- TODO add castling filtering
-- where
--   squaresToCheck

filterAllMoves :: Moves -> Position -> [MoveTypes]
filterAllMoves (Moves moves) pos = filterMoves moves pos
filterAllMoves (Sliders moves) pos = filterSlidingMoves moves pos
filterAllMoves (QueenMoves bishopMoves rookMoves) pos = filterSlidingMoves bishopMoves pos ++ filterSlidingMoves rookMoves pos
filterAllMoves (PawnMoves whiteMoves blackMoves) pos = filterPawnMoves whiteMoves pos ++ filterPawnMoves blackMoves pos
filterAllMoves (KingMoves kingMoves) pos = filterKingMoves kingMoves pos

pawnTakingMoves :: Position -> [MoveTypes]
pawnTakingMoves pos = filter pawnTake pawnMoves
  where
    likePieces = getLikePieces pos
    pawnSquares = Set.fromList $ map _square $ filter (\pieceOnSq -> view pieceTypeLens pieceOnSq == Pawn) likePieces
    pawnMoves = filter (\mv -> startingSquare mv `Set.member` pawnSquares) (_moves pos)
    oppoPieces = getOppoOccupiedSquares pos
    pawnTake mv = case attackedSquare mv of
      Nothing -> False
      Just sq -> _file sq /= _file (startingSquare mv) && sq `Set.member` oppoPieces

isTakingMove :: Position -> MoveTypes -> Bool
isTakingMove pos move = output
  where
    attackedSq = attackedSquare move
    oppoPieces = getOppoOccupiedSquares pos
    output = case attackedSq of
      Just sq -> sq `Set.member` oppoPieces
      Nothing -> False

takingMoves :: Position -> [MoveTypes]
takingMoves pos = pawnTakingMoves pos ++ filter (isTakingMove pos) nonPawnMoves
  where
    pawnSquares = Set.fromList $ map _square $ filter (\pieceOnSq -> view pieceTypeLens pieceOnSq == Pawn) (getLikePieces pos)
    nonPawnMoves = filter (\mv -> startingSquare mv `Set.notMember` pawnSquares) (_moves pos)
