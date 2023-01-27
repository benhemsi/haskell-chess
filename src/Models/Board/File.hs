{-# LANGUAGE DeriveGeneric #-}

module Models.Board.File where

import Data.Aeson
import Data.Ix
import GHC.Generics
import Test.QuickCheck
import Text.Read

data File
  = Fa
  | Fb
  | Fc
  | Fd
  | Fe
  | Ff
  | Fg
  | Fh
  deriving (Bounded, Enum, Eq, Ord, Ix, Generic)

instance ToJSON File

instance FromJSON File

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
  readPrec = do
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

predFile :: File -> File
predFile file =
  case file of
    Fa -> Fa
    _ -> pred file

succFile :: File -> File
succFile file =
  case file of
    Fh -> Fh
    _ -> succ file

instance Arbitrary File where
  arbitrary = chooseEnum (minBound, maxBound)
