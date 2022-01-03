{-# LANGUAGE DeriveDataTypeable #-}

module Models.Move where

import Data.Data
import qualified Data.Set as Set
import Models.PieceList (PieceList)
import qualified Models.PieceOnSquare as POS
import Models.Square
import Data.Maybe (fromJust)

data Move = Move {start, end :: Square} deriving (Eq, Show, Data, Typeable)

type Moves = [Move]

filterSlidingMoves :: Moves -> Squares -> Squares -> Moves
filterSlidingMoves moves likeOccupiedSquares oppoOccupiedSquare = filterMoves moves [] likeOccupiedSquares oppoOccupiedSquare
  where
    filterMoves :: Moves -> Moves -> Squares -> Squares -> Moves
    filterMoves [] curr _ _ = curr
    filterMoves (move : moves) curr likeOccupiedSquares oppoOccupiedSquare
      | end move `Set.member` likeOccupiedSquares = curr
      | end move `Set.member` oppoOccupiedSquare = move : curr
      | otherwise = filterMoves moves (move : curr) likeOccupiedSquares oppoOccupiedSquare

-- Takes a list of moves and filters out any which end on squares occupied by like pieces
filterAllMoves :: Moves -> PieceList -> Moves
filterAllMoves emptyBoardMoves likePieces = filterMoves emptyBoardMoves likeOccupiedSquares
  where
    filterMoves :: Moves -> Squares -> Moves
    filterMoves moves likePieces = foldl (\curr move -> if end move `Set.member` likePieces then curr else move : curr) [] moves

    likeOccupiedSquares = Set.fromList (map POS.square likePieces)

filterSlidingPieceMoves :: Data a => a -> Squares -> Squares -> a
filterSlidingPieceMoves moves likeOccupiedSquares oppoOccupiedSquares =
  gmapT
    ( \moves -> case cast moves of
        Just mvs -> fromJust (cast (filterSlidingMoves mvs likeOccupiedSquares oppoOccupiedSquares))
        Nothing -> moves
    ) moves
