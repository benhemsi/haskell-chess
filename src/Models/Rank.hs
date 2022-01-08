{-# LANGUAGE DeriveDataTypeable #-}

module Models.Rank
  ( Rank (..),
    predRank,
    succRank,
  )
where

import Data.Data
import Data.Ix
import Text.Read
import Text.Read.Lex (numberToInteger)

data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 deriving (Bounded, Enum, Eq, Ord, Ix, Data, Typeable)

instance Show Rank where
  show R1 = "1"
  show R2 = "2"
  show R3 = "3"
  show R4 = "4"
  show R5 = "5"
  show R6 = "6"
  show R7 = "7"
  show R8 = "8"

instance Read Rank where
  readPrec =
    do
      Number n <- lexP
      let i = numberToInteger n
      case i of
        Just 1 -> return R1
        Just 2 -> return R2
        Just 3 -> return R3
        Just 4 -> return R4
        Just 5 -> return R5
        Just 6 -> return R6
        Just 7 -> return R7
        Just 8 -> return R8
        _ -> pfail

  readListPrec = readListPrecDefault
  readList = readListDefault

predRank rank = case rank of
  R1 -> R1
  _ -> pred rank

succRank rank = case rank of
  R8 -> R8
  _ -> succ rank
