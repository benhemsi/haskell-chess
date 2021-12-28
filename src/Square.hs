module Square (Rank, File, Square) where

data File = Fa | Fb | Fc | Fd | Fe | Ff | Fg | Fh deriving (Enum, Eq, Ord)
data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 deriving (Enum, Eq, Ord)
data Square = Square { file :: File, rank :: Rank } deriving (Eq, Ord)

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
  show (Square file r) = show file ++ show r
