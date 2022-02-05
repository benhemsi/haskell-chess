{-# LANGUAGE TemplateHaskell #-}

module Models.Square where

import Control.Lens
import Data.Ix
import Data.List (elemIndex)
import qualified Data.Set as Set
import Models.File
import Models.Rank
import Text.Read

data Square = Square {_file :: File, _rank :: Rank} deriving (Eq)

makeLenses ''Square

type Squares = Set.Set Square

instance Show Square where
  show (Square file rank) = show file ++ show rank

instance Read Square where
  readPrec =
    do
      Ident (f : r) <- lexP
      let file = readMaybe [f]
          rank = readMaybe r
      case (file, rank) of
        (Just f', Just r') -> return (Square f' r')
        _ -> pfail

  readListPrec = readListPrecDefault
  readList = readListDefault

instance Enum Square where
  toEnum i =
    let file = toEnum (i `rem` 8)
        rank = toEnum (i `div` 8)
     in Square file rank
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
