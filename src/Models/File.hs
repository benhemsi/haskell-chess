module Models.File
  ( File (Fa, Fb, Fc, Fd, Fe, Ff, Fg, Fh),
    predFile,
    succFile,
  )
where

import Data.Ix
import Text.Read

data File = Fa | Fb | Fc | Fd | Fe | Ff | Fg | Fh deriving (Bounded, Enum, Eq, Ord, Ix)

instance Show File where
  show Fa = "a"
  show Fb = "b"
  show Fc = "c"
  show Fd = "d"
  show Fe = "e"
  show Ff = "f"
  show Fg = "g"
  show Fh = "h"

instance Read File where
  readPrec =
    do
      Ident s <- lexP
      case s of
        "a" -> return Fa
        "b" -> return Fb
        "c" -> return Fc
        "d" -> return Fd
        "e" -> return Fe
        "f" -> return Ff
        "g" -> return Fg
        "h" -> return Fh
        _ -> pfail

  readListPrec = readListPrecDefault
  readList = readListDefault

predFile file = case file of
  Fa -> Fa
  _ -> pred file

succFile file = case file of
  Fh -> Fh
  _ -> succ file
