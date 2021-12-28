{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Moves(emptyBoardMoves) where
import Square
import Piece
import Data.Ix (range)
import Data.Array (Array, array, (!))

data Move = Move { start, end  :: Square }
type Moves = [Move]

type KingMoves = Moves
data RookMoves = RookMoves { north, east, south, west :: Moves }
data BishopMoves = BishopMoves { northEast, southEast, southWest, northWest :: Moves }
data QueenMoves = QueenMoves { rookMoves :: RookMoves, bishopMoves :: BishopMoves }
type KnightMoves = Moves
data PawnMoves = PawnMoves { white :: Moves, black :: Moves }

data PieceMoves = PieceMoves
  { kingMoves :: KingMoves,
    queenMoves :: QueenMoves,
    rookMoves :: RookMoves,
    bishopMoves :: BishopMoves,
    knightMoves :: KnightMoves,
    pawnMoves :: PawnMoves
  }

type Board = Array Square

class Representable f where
  type Rep f :: *
  tabulate :: (Rep f -> a) -> f a
  index :: f a -> Rep f -> a

getKingMoves :: Square -> KingMoves
getKingMoves start =
  let startFile = file start
      startRank = rank start
      minFile = predFile startFile
      maxFile = succFile startFile
      minRank = predRank startRank
      maxRank = succRank startRank
   in [ Move start endSq
        | f <- [minFile .. maxFile],
          r <- [minRank .. maxRank],
          let endSq = square f r,
          endSq /= start
      ]

getQueenMoves start = QueenMoves (getRookMoves start) (getBishopMoves start)

getRookMoves start =
  let startFile = file start
      startRank = rank start
      north = if startRank == R8 then [] else map (Move start . square startFile) (getRange (succ startRank) R8)
      south = if startRank == R1 then [] else map (Move start . square startFile) (getRange (pred startRank) R1)
      east = if startFile == Fh then [] else map (Move start . square' startRank) (getRange (succ startFile) Fh)
      west = if startFile == Fa then [] else map (Move start . square' startRank) (getRange (pred startFile) Fa)
   in RookMoves north east south west

getBishopMoves start =
  let startFile = file start
      startRank = rank start
      northEast = getDiagonal start (square Fh R8)
      southEast = getDiagonal start (square Fh R1)
      southWest = getDiagonal start (square Fa R1)
      northWest = getDiagonal start (square Fa R8)
   in BishopMoves northEast southEast southWest northWest

getDiagonal start target =
  let startFile = file start
      startRank = rank start
      targetFile = file target
      targetRank = rank target
   in ( if (startFile == targetFile) || (startRank == targetRank)
          then []
          else
            zipWith
              (curry (Move start . uncurry square))
              (getRange startFile targetFile)
              (getRange startRank targetRank)
      )

getKnightMoves :: Square -> KnightMoves
getKnightMoves start = 
  let startFile = fromEnum (file start)
      startRank = fromEnum (rank start)
      in [Move start (square (toEnum f) (toEnum r)) | 
        i <- [-2,-1,1,2],
        j <- [-2,-1,1,2],
        abs (i * j) == 2,
        let f = startFile + i
            r = startRank + j,
        f >= 0 && f <= 8,
        r >= 0 && r <= 8]
        
getPawnMoves start =
  let startRank = rank start
   in ( if startRank == R1 || startRank == R8
          then PawnMoves [] []
          else
            let startFile = file start
                forwardWhite = Move start (square startFile (succ startRank))
                forwardBlack = Move start (square startFile (pred startRank))
                takeLeftWhite = [Move start (square (pred startFile) (succ startRank)) | startFile /= Fa]
                takeLeftBlack = [Move start (square (pred startFile) (pred startRank)) | startFile /= Fa]
                takeRightWhite = [Move start (square (succ startFile) (succ startRank)) | startFile /= Fh]
                takeRightBlack = [Move start (square (succ startFile) (pred startRank)) | startFile /= Fh]
             in PawnMoves (forwardWhite : takeLeftWhite ++ takeRightWhite) (forwardBlack : takeLeftBlack ++ takeRightBlack)
      )

getPieceMoves start = PieceMoves (getKingMoves start) (getQueenMoves start) (getRookMoves start) (getBishopMoves start) (getKnightMoves start) (getPawnMoves start)

moveBoard :: Board PieceMoves
moveBoard = tabulate getPieceMoves

instance Representable Board where
  type Rep Board = Square
  tabulate f = array (minBound, maxBound) [(i, f i) | i <- range (minBound, maxBound)]
  index = (!)

emptyBoardMoves = index moveBoard
