{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-# HLINT ignore "Use <$>" #-}
module Models.CastlingPrivileges where

import Control.Lens
import qualified Data.Set as Set
import Models.File
import Models.FullPieceList
import Models.Move
import Models.PieceColour
import Models.Rank
import Models.Square
import Test.QuickCheck
import Text.RawString.QQ (r)
import Text.Read
import Text.Regex.TDFA

data CastlingPrivileges =
  CastlingPrivileges
    { _whiteKingSide, _whiteQueenSide, _blackKingSide, _blackQueenSide :: Bool
    }
  deriving (Eq)

makeLenses ''CastlingPrivileges

instance Show CastlingPrivileges where
  show (CastlingPrivileges wk wq bk bq) =
    case [c | (bool, c) <- zip [wk, wq, bk, bq] "KQkq", bool] of
      [] -> "-"
      s -> s

regexMatch :: String -> Bool
regexMatch s = s =~ [r|^(-|(K?Q?k?q?))$|]

instance Read CastlingPrivileges where
  readPrec =
    (do Symbol "-" <- lexP
        return (CastlingPrivileges False False False False)) +++
    (do Ident s <- lexP
        if regexMatch s
          then return (CastlingPrivileges ('K' `elem` s) ('Q' `elem` s) ('k' `elem` s) ('q' `elem` s))
          else pfail)
  readListPrec = readListPrecDefault
  readList = readListDefault

getKingSideCastle :: PieceColour -> Castle
getKingSideCastle colour = Castle (Move (Square Fe rank) (Square Fg rank)) (Move (Square Fh rank) (Square Ff rank))
  where
    rank =
      case colour of
        White -> R1
        Black -> R8

getQueenSideCastle :: PieceColour -> Castle
getQueenSideCastle colour = Castle (Move (Square Fe rank) (Square Fc rank)) (Move (Square Fa rank) (Square Fd rank))
  where
    rank =
      case colour of
        White -> R1
        Black -> R8

instance Arbitrary CastlingPrivileges where
  arbitrary = do
    wk <- arbitrary
    wq <- arbitrary
    bk <- arbitrary
    bq <- arbitrary
    return $ CastlingPrivileges wk wq bk bq
