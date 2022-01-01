module Models.File (
  File (Fa, Fb, Fc, Fd, Fe, Ff, Fg, Fh),
  predFile,
  succFile,
                   ) where
import Data.Ix

data File = Fa | Fb | Fc | Fd | Fe | Ff | Fg | Fh deriving (Enum, Eq, Ord, Ix)

instance Show File where
  show Fa = "a"
  show Fb = "b"
  show Fc = "c"
  show Fd = "d"
  show Fe = "e"
  show Ff = "f"
  show Fg = "g"
  show Fh = "h"

predFile file = case file of
  Fa -> Fa
  _ -> pred file

succFile file = case file of
  Fh -> Fh
  _ -> succ file

