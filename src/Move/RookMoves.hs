{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Move.RookMoves where

import Data.Data
import Data.Functor.Identity
import Data.Maybe
import qualified Data.Set as Set
import Models.File
import Models.Move
import Models.PieceList
import qualified Models.PieceOnSquare as POS
import Models.Rank
import Models.Square

data RookMoves = RookMoves {north, east, south, west :: Moves} deriving (Show, Data, Typeable)

emptyBoardMoves :: Square -> RookMoves
emptyBoardMoves start =
  let startFile = file start
      startRank = rank start
      north = if startRank == R8 then [] else map (Move start . square startFile) (getRange (succ startRank) R8)
      south = if startRank == R1 then [] else map (Move start . square startFile) (getRange (pred startRank) R1)
      east = if startFile == Fh then [] else map (Move start . square' startRank) (getRange (succ startFile) Fh)
      west = if startFile == Fa then [] else map (Move start . square' startRank) (getRange (pred startFile) Fa)
   in RookMoves north east south west

flattenMoves :: RookMoves -> Moves
flattenMoves rookMoves = north rookMoves ++ east rookMoves ++ south rookMoves ++ west rookMoves

validMoves :: RookMoves -> Squares -> Squares -> Moves
validMoves rookMoves likeOccupiedSquares oppoOccupiedSquares = flattenMoves filteredRookMoves where
  filteredRookMoves = filterSlidingPieceMoves rookMoves likeOccupiedSquares oppoOccupiedSquares
