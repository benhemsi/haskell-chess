module Models.Rank (
    Rank (R1, R2, R3, R4, R5, R6, R7, R8),
    predRank,
    succRank,
                   ) where
import Data.Ix

data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 deriving (Enum, Eq, Ord, Ix)

instance Show Rank where
  show R1 = "1"
  show R2 = "2"
  show R3 = "3"
  show R4 = "4"
  show R5 = "5"
  show R6 = "6"
  show R7 = "7"
  show R8 = "8"

predRank rank = case rank of
  R1 -> R1
  _ -> pred rank

succRank rank = case rank of
  R8 -> R8
  _ -> succ rank

