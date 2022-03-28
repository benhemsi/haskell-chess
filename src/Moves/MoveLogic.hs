module Moves.MoveLogic where

import Control.Lens
import qualified Data.Set as Set
import Data.Foldable (toList)
import Models.FenRepresentation
import Models.FullPieceList
import Models.Move
import Models.PieceColour
import Models.PieceOnSquare
import Models.PieceType
import Models.Position
import Models.Square
import Models.Piece

flattenMoves :: Moves -> [MoveTypes]
flattenMoves (Moves mvs) = map Mv mvs
flattenMoves (Sliders (SlidingMoves a b c d)) = map Mv (a ++ b ++ c ++ d)
flattenMoves (QueenMoves b r) = flattenMoves (Sliders b) ++ flattenMoves (Sliders r)
flattenMoves (KingMoves (KM mvs kc qc)) = map Mv mvs ++ map Cst (toList kc) ++ map Cst (toList qc)
flattenMoves (PawnMoves w b) = whiteMoves ++ blackMoves
  where
    flattenPawnMoves (PM f j t enP pr prTks) = map Mv (toList f ++ toList j ++ t) ++ map EnP enP ++ map (\(PawnPromotion mv) -> PP mv) (toList pr ++ prTks)
    whiteMoves = flattenPawnMoves w
    blackMoves = flattenPawnMoves b

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
    filterFunction (Move _ end) = end `Set.notMember` likePieces
    filterMoves :: [Move] -> [MoveTypes]
    filterMoves moves = movesToMoveTypes $ filter filterFunction emptyBoardMoves

filterPawnMoves :: PawnMoves -> Position -> [MoveTypes]
filterPawnMoves (PM f1 f2 tks enPs pr prTks) pos = movesToMoveTypes (forward ++ takes) ++ enPassent ++ promotion ++ promotionTakes
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

    enPassent = case view (fen . enPassentSquare) pos of
      Just sq -> map EnP $ filter (\(EnPassent move _) -> _end move == sq) enPs
      Nothing -> []

    promotion = toList $ fmap PP (filterMove $ fmap (\(PawnPromotion move) -> move) pr)
    promotionTakes = map (\(PawnPromotion mv) -> PP mv) $ filter (\(PawnPromotion mv) -> _end mv `Set.member` oppoPieces) prTks

filterKingMoves :: KingMoves -> Position -> [MoveTypes]
filterKingMoves (KM moves kingSide queenSide) = filterMoves moves -- TODO add castling filtering
-- where
--   squaresToCheck

filterAllMoves :: Moves -> Position -> [MoveTypes]
filterAllMoves (Moves moves) pos = filterMoves moves pos
filterAllMoves (Sliders moves) pos = filterSlidingMoves moves pos
filterAllMoves (QueenMoves bishopMoves rookMoves) pos = filterSlidingMoves bishopMoves pos ++ filterSlidingMoves rookMoves pos
filterAllMoves (PawnMoves whiteMoves blackMoves) pos = case view (fen . nextToMove) pos of
  White -> filterPawnMoves whiteMoves pos
  Black -> filterPawnMoves blackMoves pos
filterAllMoves (KingMoves kingMoves) pos = filterKingMoves kingMoves pos

isTakingMove :: Position -> MoveTypes -> Bool
isTakingMove pos move = output
  where
    attackedSq = attackedSquare move
    oppoPieces = getOppoOccupiedSquares pos
    output = case attackedSq of
      Just sq -> sq `Set.member` oppoPieces
      Nothing -> False

getEmptyBoardMoves :: PieceOnSquare -> Moves
getEmptyBoardMoves (PieceOnSquare tpe sq) = emptyBoardMoves tpe sq

getValidMoves :: PieceColour -> Position -> [(PieceOnSquare, [MoveTypes])]
getValidMoves col pos = [(piece, filterAllMoves moves pos) | piece <- likePieces, let moves = getEmptyBoardMoves piece]
  where
    likePieces = case col of
                   White -> view (pieceList . whitePieces) pos
                   Black -> view (pieceList . blackPieces) pos

getLikeValidMoves :: Position -> [(PieceOnSquare, [MoveTypes])]
getLikeValidMoves pos = getValidMoves (view (fen . nextToMove) pos) pos

getOppoValidMoves :: Position -> [(PieceOnSquare, [MoveTypes])]
getOppoValidMoves pos = getValidMoves (oppoColour (view (fen . nextToMove) pos)) pos
