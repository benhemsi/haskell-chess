{-# LANGUAGE FlexibleInstances #-}

module Moves.MoveFiltering where

import Control.Lens
import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Models.FenRepresentation
import Models.File
import Models.FullPieceList
import Models.Move
import Models.PieceColour
import Models.PieceOnSquare
import Models.Position
import Models.Rank
import Models.Square

class MoveFiltering mvs where
  filterMoves :: mvs -> Position -> [MoveTypes]

instance MoveFiltering SlidingMoves where
  filterMoves (SlidingMoves a b c d) pos = movesToMoveTypes $ foldMoves a ++ foldMoves b ++ foldMoves c ++ foldMoves d
    where
      likePieces = getLikeOccupiedSquares pos
      oppoPieces = getOppoOccupiedSquares pos
      recursiveFilter :: [Move] -> [Move] -> [Move]
      recursiveFilter [] curr = curr
      recursiveFilter (move:moves) curr
        | _end move `Set.member` likePieces = curr
        | _end move `Set.member` oppoPieces = move : curr
        | otherwise = recursiveFilter moves (move : curr)
      foldMoves :: [Move] -> [Move]
      foldMoves moves = recursiveFilter moves []

instance MoveFiltering [Move] where
  filterMoves emptyBoardMvs pos = output
    where
      likePieces = getLikeOccupiedSquares pos
      filterFunction :: Move -> Bool
      filterFunction (Move _ endSq) = endSq `Set.notMember` likePieces
      output = movesToMoveTypes $ filter filterFunction emptyBoardMvs

instance MoveFiltering PawnMoves where
  filterMoves (PM f1 f2 tks enPs pr prTks) pos =
    movesToMoveTypes (forwardMoves ++ takingMoves) ++ enPassentMove ++ promotionMoves ++ promotionTks
    where
      likePieces = getLikeOccupiedSquares pos
      oppoPieces = getOppoOccupiedSquares pos
      allPieces = likePieces `Set.union` oppoPieces
      filterMove :: Maybe Move -> [Move]
      filterMove Nothing = []
      filterMove (Just move) = [move | _end move `Set.notMember` allPieces]
      forward1 = filterMove f1
      forwardMoves =
        case (forward1, f2) of
          ([], _) -> forward1
          _ -> forward1 ++ filterMove f2
      takingMoves = filter (\move -> _end move `Set.member` oppoPieces) tks
      enPassentMove =
        case view (fen . enPassentSquare) pos of
          Just sq -> map EnP $ filter (\(EnPassent move _) -> _end move == sq) enPs
          Nothing -> []
      promotionMoves = toList $ fmap (PP . PawnPromotion) (filterMove $ fmap (\(PawnPromotion move) -> move) pr)
      promotionTks = map PP $ filter (\(PawnPromotion mv) -> _end mv `Set.member` oppoPieces) prTks

instance MoveFiltering KingMoves where
  filterMoves (KM moves kingSide queenSide) pos = filterMoves moves pos ++ kingSideMove ++ queenSideMove
    where
      attackedSquares = getOppoAttackedSquares pos
      occupiedSquares = getLikeOccupiedSquares pos `Set.union` getOppoOccupiedSquares pos
      rankToCheck =
        case view (fen . nextToMove) pos of
          White -> R1
          Black -> R8
      kingSideMove =
        if isJust kingSide && getKingSidePrivileges pos
          then let filesToCheck = [Ff, Fg]
                   squaresToCheck = Set.fromList $ map (`Square` rankToCheck) filesToCheck
                   allSquaresToCheck = Set.union occupiedSquares attackedSquares
                   cond = squaresToCheck `Set.disjoint` allSquaresToCheck
                in if cond
                     then toList $ fmap Cst kingSide
                     else []
          else []
      queenSideMove =
        if isJust queenSide && getQueenSidePrivileges pos
          then let filesToCheckIfAttacked = [Fc, Fd]
                   filesToCheckIfOccupied = Fb : filesToCheckIfAttacked
                   squaresToCheckIfOccupied = Set.fromList $ map (`Square` rankToCheck) filesToCheckIfOccupied
                   squaresToCheckIfAttacked = Set.fromList $ map (`Square` rankToCheck) filesToCheckIfAttacked
                   cond =
                     squaresToCheckIfOccupied `Set.disjoint` occupiedSquares &&
                     squaresToCheckIfAttacked `Set.disjoint` attackedSquares
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

getAttackedSquares :: Moves -> Position -> [Square]
getAttackedSquares (Moves mvs) _ = flattenAttackedSquares (flattenMoves $ Moves mvs)
getAttackedSquares (Sliders slidingMoves) pos = flattenAttackedSquares $ filterMoves slidingMoves (switchNextToMove pos)
getAttackedSquares (QueenMoves b r) pos = getAttackedSquares (Sliders b) pos ++ getAttackedSquares (Sliders r) pos
getAttackedSquares (KingMoves (KM mvs _ _)) pos = getAttackedSquares (Moves mvs) pos
getAttackedSquares (PawnMoves w b) pos = squares
  where
    getSquares (PM _ _ tks enPs _ prTks) =
      getAttackedSquares
        (Moves $ tks ++ map (\(EnPassent mv _) -> mv) enPs ++ map (\(PawnPromotion mv) -> mv) prTks)
        pos
    squares =
      case view (fen . nextToMove) pos of
        White -> getSquares b
        Black -> getSquares w

getOppoAttackedSquares :: Position -> Squares
getOppoAttackedSquares pos = Set.fromList squares
  where
    oppoPieces = getOppoPieces pos
    squares = concatMap ((`getAttackedSquares` pos) . getEmptyBoardMoves) oppoPieces

movesToMoveTypes :: [Move] -> [MoveTypes]
movesToMoveTypes = map Mv

isTakingMove :: Position -> MoveTypes -> Bool
isTakingMove pos move = output
  where
    attackedSq = attackedSquare move
    oppoPieces = getOppoOccupiedSquares pos
    output =
      case attackedSq of
        Just sq -> sq `Set.member` oppoPieces
        Nothing -> False

getEmptyBoardMoves :: PieceOnSquare -> Moves
getEmptyBoardMoves (PieceOnSquare tpe sq) = emptyBoardMoves tpe sq

getValidMoves :: PieceColour -> Position -> Map.Map PieceOnSquare [MoveTypes]
getValidMoves col pos =
  Map.fromList [(pce, filterMoves moves pos) | pce <- likePieces, let moves = getEmptyBoardMoves pce]
  where
    likePieces =
      case col of
        White -> view (pieceList . whitePieces) pos
        Black -> view (pieceList . blackPieces) pos

getLikeValidMoves :: Position -> Map.Map PieceOnSquare [MoveTypes]
getLikeValidMoves pos = getValidMoves (view (fen . nextToMove) pos) pos

getOppoValidMoves :: Position -> Map.Map PieceOnSquare [MoveTypes]
getOppoValidMoves pos = getValidMoves (oppoColour (view (fen . nextToMove) pos)) pos
