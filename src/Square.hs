module Square
  ( File (Fa, Fb, Fc, Fd, Fe, Ff, Fg, Fh),
    Rank (R1, R2, R3, R4, R5, R6, R7, R8),
    Square,
    square,
    square',
    file,
    rank,
    predFile,
    predRank,
    succFile,
    succRank,
    getRange,
  )
where
import Data.Ix
import Data.List (elemIndex)

data File = Fa | Fb | Fc | Fd | Fe | Ff | Fg | Fh deriving (Enum, Eq, Ord, Ix)
data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 deriving (Enum, Eq, Ord, Ix)
data Square = Square { file :: File, rank :: Rank } deriving Eq
square = Square
square' rank file = Square file rank

instance Show File where
  show Fa = "a"
  show Fb = "b"
  show Fc = "c"
  show Fd = "d"
  show Fe = "e"
  show Ff = "f"
  show Fg = "g"
  show Fh = "h"

instance Show Rank where
  show R1 = "1"
  show R2 = "2"
  show R3 = "3"
  show R4 = "4"
  show R5 = "5"
  show R6 = "6"
  show R7 = "7"
  show R8 = "8"

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
  range (minSq, maxSq) = [minSq..maxSq]
  index (minSq, maxSq) s =
    case elemIndex s (range (minSq, maxSq)) of
      Just i -> i
      Nothing -> error "Square outside range"
  inRange (minSq, maxSq) s = s `elem` range (minSq, maxSq)

predFile file = case file of
  Fa -> Fa
  _ -> pred file

succFile file = case file of
  Fh -> Fh
  _ -> succ file

predRank rank = case rank of
  R1 -> R1
  _ -> pred rank

succRank rank = case rank of
  R8 -> R8
  _ -> succ rank

getRange :: (Ord a, Ix a) => a -> a -> [a]
getRange start end =
  if start <= end
    then range (start, end)
    else reverse (range (end, start))
