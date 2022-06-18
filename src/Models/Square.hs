{-# LANGUAGE TemplateHaskell #-}

module Models.Square where

import Control.Lens
import Data.Ix
import Data.List (elemIndex)
import qualified Data.Set as Set
import Models.File
import Models.Rank
import Test.QuickCheck
import Text.Read

data Square =
  Square
    { _file :: File
    , _rank :: Rank
    }
  deriving (Eq)

makeLenses ''Square

type Squares = Set.Set Square

instance Show Square where
  show (Square fle rnk) = show fle ++ show rnk

instance Read Square where
  readPrec = do
    Ident (f:r) <- lexP
    let fle = readMaybe [f]
        rnk = readMaybe r
    case (fle, rnk) of
      (Just f', Just r') -> return (Square f' r')
      _ -> pfail
  readListPrec = readListPrecDefault
  readList = readListDefault

instance Enum Square where
  toEnum i =
    let fle = toEnum (i `rem` 8)
        rnk = toEnum (i `div` 8)
     in Square fle rnk
  fromEnum (Square fle rnk) = 8 * fromEnum rnk + fromEnum fle

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

getRange :: Ix a => a -> a -> [a]
getRange start end =
  if start <= end
    then range (start, end)
    else reverse (range (end, start))

instance Arbitrary Square where
  arbitrary = chooseEnum (minBound, maxBound)
