{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Chess.Moves.MoveFiltering where

import Chess.Board
import Chess.Fen
import Chess.Move
import Chess.Piece
import Chess.Position
import Control.Lens
import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set

class MoveFiltering mvs where
  filterMoves :: mvs -> Position -> [MoveTypes]

instance MoveFiltering SlidingMoves where
  filterMoves (SlidingMoves a b c d) pos = movesToMoveTypes $ foldMoves a ++ foldMoves b ++ foldMoves c ++ foldMoves d
    where
      recursiveFilter :: [Move] -> [Move] -> [Move]
      recursiveFilter [] curr = curr
      recursiveFilter (move:moves) curr
        | _end move `Set.member` view likeOccupiedSquares pos = curr
        | _end move `Set.member` view oppoOccupiedSquares pos = move : curr
        | otherwise = recursiveFilter moves (move : curr)
      foldMoves :: [Move] -> [Move]
      foldMoves moves = recursiveFilter moves []

instance MoveFiltering [Move] where
  filterMoves emptyBoardMvs pos = output
    where
      filterFunction :: Move -> Bool
      filterFunction (Move _ endSq) = endSq `Set.notMember` view likeOccupiedSquares pos
      output = movesToMoveTypes $ filter filterFunction emptyBoardMvs

instance MoveFiltering PawnMoves where
  filterMoves (PM f1 f2 tks enPs pr prTks) pos =
    movesToMoveTypes (forwardMoves ++ takingMoves) ++ enPassentMove ++ promotionMoves ++ promotionTks
    where
      oppoSquares = view oppoOccupiedSquares pos
      allPieces = view likeOccupiedSquares pos `Set.union` oppoSquares
      filterMove :: Maybe Move -> [Move]
      filterMove Nothing = []
      filterMove (Just move) = [move | _end move `Set.notMember` allPieces]
      forward1 = filterMove f1
      forwardMoves =
        case (forward1, f2) of
          ([], _) -> forward1
          _ -> forward1 ++ filterMove f2
      takingMoves = filter (\move -> _end move `Set.member` oppoSquares) tks
      enPassentMove =
        case view (fen . enPassentSquare . enPassentSq) pos of
          Just sq -> map EnP $ filter (\(EnPassent move _) -> _end move == sq) enPs
          Nothing -> []
      promotionMoves = toList $ fmap (PP . PawnPromotion) (filterMove $ fmap (\(PawnPromotion move) -> move) pr)
      promotionTks = map PP $ filter (\(PawnPromotion mv) -> _end mv `Set.member` oppoSquares) prTks

instance MoveFiltering KingMoves where
  filterMoves (KM moves kingSide queenSide) pos = filterMoves moves pos ++ kingSideMove ++ queenSideMove
    where
      attackedSquares = getOppoAttackedSquares pos
      occupiedSquares = view likeOccupiedSquares pos `Set.union` view oppoOccupiedSquares pos
      rankToCheck =
        case view (fen . nextToMove) pos of
          White -> R1
          Black -> R8
      kingSideMove =
        if isJust kingSide && view kingSidePrivileges pos
          then let filesToCheck = [Ff, Fg]
                   squaresToCheck = Set.fromList $ map (`Square` rankToCheck) filesToCheck
                   allSquaresToCheck = Set.union occupiedSquares attackedSquares
                   cond = squaresToCheck `Set.disjoint` allSquaresToCheck
                in if cond
                     then toList $ fmap Cst kingSide
                     else []
          else []
      queenSideMove =
        if isJust queenSide && view queenSidePrivileges pos
          then let filesToCheckIfAttacked = [Fc, Fd]
                   filesToCheckIfOccupied = Fb : filesToCheckIfAttacked
                   squaresToCheckIfOccupied = Set.fromList $ map (`Square` rankToCheck) filesToCheckIfOccupied
                   squaresToCheckIfAttacked = Set.fromList $ map (`Square` rankToCheck) filesToCheckIfAttacked
                   cond =
                     squaresToCheckIfOccupied `Set.disjoint` occupiedSquares && squaresToCheckIfAttacked `Set.disjoint`
                     attackedSquares
                in if cond
                     then toList $ fmap Cst queenSide
                     else []
          else []

instance MoveFiltering Moves where
  filterMoves (Moves moves) pos = filterMoves moves pos
  filterMoves (Sliders moves) pos = filterMoves moves pos
  filterMoves (QueenMoves bishopMvs rookMvs) pos = filterMoves bishopMvs pos ++ filterMoves rookMvs pos
  filterMoves (PawnMoves whiteMoves blackMoves) pos =
    case view (fen . nextToMove) pos of
      White -> filterMoves whiteMoves pos
      Black -> filterMoves blackMoves pos
  filterMoves (KingMoves kingMoves) pos = filterMoves kingMoves pos

flattenMoves :: Moves -> [MoveTypes]
flattenMoves (Moves mvs) = map Mv mvs
flattenMoves (Sliders (SlidingMoves a b c d)) = map Mv (a ++ b ++ c ++ d)
flattenMoves (QueenMoves b r) = flattenMoves (Sliders b) ++ flattenMoves (Sliders r)
flattenMoves (KingMoves (KM mvs kc qc)) = map Mv mvs ++ map Cst (toList kc) ++ map Cst (toList qc)
flattenMoves (PawnMoves w b) = whiteMoves ++ blackMoves
  where
    flattenPawnMoves (PM f j t enP pr prTks) =
      map Mv (toList f ++ toList j ++ t) ++ map EnP enP ++ map PP (toList pr ++ prTks)
    whiteMoves = flattenPawnMoves w
    blackMoves = flattenPawnMoves b

flattenAttackedSquares :: [MoveTypes] -> [Square]
flattenAttackedSquares = concatMap (toList . attackedSquare)

getOppoAttackedSquares :: Position -> Squares
getOppoAttackedSquares pos = foldr (\mvs curr -> Set.union curr (getAttackedSquares pos mvs)) Set.empty oppoMoves
  where
    oppoMoves = [emptyBoardMoves pce sq | (sq, pce) <- Map.assocs $ view oppoPieces pos]

movesToMoveTypes :: [Move] -> [MoveTypes]
movesToMoveTypes = map Mv

isTakingMove :: Position -> MoveTypes -> Bool
isTakingMove pos move = output
  where
    attackedSq = attackedSquare move
    output =
      case attackedSq of
        Just sq -> sq `Set.member` view oppoOccupiedSquares pos
        Nothing -> False

getValidMoves :: Lens' Position (Map.Map Square PieceType) -> Position -> Map.Map PieceOnSquare [MoveTypes]
getValidMoves plLens pos =
  Map.fromList
    [ (PieceOnSquare (Piece (pos ^. fen . nextToMove) pce) sq, filterMoves moves pos)
    | (sq, pce) <- Map.assocs $ view plLens pos
    , let moves = emptyBoardMoves pce sq
    ]

getLikeValidMoves :: Position -> Map.Map PieceOnSquare [MoveTypes]
getLikeValidMoves = getValidMoves likePieces

getOppoValidMoves :: Position -> Map.Map PieceOnSquare [MoveTypes]
getOppoValidMoves = getValidMoves oppoPieces

-- This should return all squares which a king can't move to. This includes all squares a night can hop, all sliding moves up to and including the final piece, all king moves, and pawn taking moves.
getAttackedSquares :: Position -> Moves -> Squares
getAttackedSquares pos (PawnMoves whiteMoves blackMoves) =
  Set.fromList $ flattenAttackedSquares (map Mv (takes pawnMoves)) ++
  map (\(PawnPromotion mv) -> _end mv) (promotionTakes pawnMoves)
  where
    pawnMoves =
      case view (fen . nextToMove) pos of
        White -> blackMoves
        Black -> whiteMoves
getAttackedSquares pos moves = Set.fromList $ flattenAttackedSquares (filterMoves moves pos)
