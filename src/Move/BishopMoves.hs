module Move.BishopMoves where

import Models.File
import Models.Move
import Models.Rank
import Models.Square

data BishopMoves = BishopMoves {northEast, southEast, southWest, northWest :: Moves} deriving (Show)

emptyBoardMoves :: Square -> BishopMoves
emptyBoardMoves start = BishopMoves northEast southEast southWest northWest
  where
    getDiagonal start target =
      let startFile = file start
          startRank = rank start
          targetFile = file target
          targetRank = rank target
       in zipWith
            (curry (Move start . uncurry square))
            (getRange startFile targetFile)
            (getRange startRank targetRank)
    startFile = file start
    startRank = rank start
    northEast = if (startFile /= Fh) && (startRank /= R8) then getDiagonal (square (succ startFile) (succ startRank)) (square Fh R8) else []
    southEast = if (startFile /= Fh) && (startRank /= R1) then getDiagonal (square (succ startFile) (pred startRank)) (square Fh R1) else []
    southWest = if (startFile /= Fa) && (startRank /= R1) then getDiagonal (square (pred startFile) (pred startRank)) (square Fa R1) else []
    northWest = if (startFile /= Fa) && (startRank /= R8) then getDiagonal (square (pred startFile) (succ startRank)) (square Fa R8) else []

flattenMoves :: BishopMoves -> Moves
flattenMoves bishopMoves = northEast bishopMoves ++ southEast bishopMoves ++ southWest bishopMoves ++ northWest bishopMoves
