module Models.Square
  ( Square,
    square,
    square',
    file,
    rank,
    getRange,
  )
where

import Data.Ix
import Data.List (elemIndex)
import Models.File
import Models.Rank

data Square = Square {file :: File, rank :: Rank} deriving (Eq)

square :: File -> Rank -> Square
square = Square

square' :: Rank -> File -> Square
square' = flip square

instance Show Square where
  show (Square file rank) = show file ++ show rank

instance Enum Square where
  toEnum i =
    let file = toEnum (i `rem` 8)
        rank = toEnum (i `div` 8)
     in square file rank
  fromEnum (Square file rank) = 8 * fromEnum rank + fromEnum file

instance Ord Square where
  (<=) sq1 sq2 = fromEnum sq1 <= fromEnum sq2

instance Bounded Square where
  minBound = Square Fa R1
  maxBound = Square Fh R8

instance Ix Square where
  range (minSq, maxSq) = [minSq .. maxSq]
  index (minSq, maxSq) s =
    case elemIndex s (range (minSq, maxSq)) of
      Just i -> i
      Nothing -> error "Square outside range"
  inRange (minSq, maxSq) s = s `elem` range (minSq, maxSq)

getRange :: (Ord a, Ix a) => a -> a -> [a]
getRange start end =
  if start <= end
    then range (start, end)
    else reverse (range (end, start))
