{-# LANGUAGE DeriveDataTypeable #-}

module Move.BishopMoves where

import Data.Data
import Models.File
import Models.Move
import Models.PieceList
import Models.Rank
import Models.Square

data BishopMoves = BishopMoves {northEast, southEast, southWest, northWest :: Moves} deriving (Show, Data, Typeable)

emptyBoardMoves :: Square -> BishopMoves
emptyBoardMoves start = BishopMoves northEast southEast southWest northWest
  where
    getDiagonal start target =
      let startFile = file start
          startRank = rank start
          targetFile = file target
          targetRank = rank target
       in zipWith
            (curry (Move start . uncurry Square))
            (getRange startFile targetFile)
            (getRange startRank targetRank)
    startFile = file start
    startRank = rank start
    northEast = if (startFile /= Fh) && (startRank /= R8) then getDiagonal (Square (succ startFile) (succ startRank)) (Square Fh R8) else []
    southEast = if (startFile /= Fh) && (startRank /= R1) then getDiagonal (Square (succ startFile) (pred startRank)) (Square Fh R1) else []
    southWest = if (startFile /= Fa) && (startRank /= R1) then getDiagonal (Square (pred startFile) (pred startRank)) (Square Fa R1) else []
    northWest = if (startFile /= Fa) && (startRank /= R8) then getDiagonal (Square (pred startFile) (succ startRank)) (Square Fa R8) else []

flattenMoves :: BishopMoves -> Moves
flattenMoves bishopMoves = northEast bishopMoves ++ southEast bishopMoves ++ southWest bishopMoves ++ northWest bishopMoves

validMoves :: BishopMoves -> Squares -> Squares -> Moves
validMoves bishopMoves likeOccupiedSquares oppoOccupiedSquares = flattenMoves filteredBishopMoves
  where
    filteredBishopMoves = filterSlidingPieceMoves bishopMoves likeOccupiedSquares oppoOccupiedSquares
