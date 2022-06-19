{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-# HLINT ignore "Use <$>" #-}
module Models.CastlingPrivileges where

import Control.Lens
import Models.Board
import Models.Move
import Models.Piece
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
getKingSideCastle colour = Castle (Move (Square Fe rnk) (Square Fg rnk)) (Move (Square Fh rnk) (Square Ff rnk))
  where
    rnk =
      case colour of
        White -> R1
        Black -> R8

getQueenSideCastle :: PieceColour -> Castle
getQueenSideCastle colour = Castle (Move (Square Fe rnk) (Square Fc rnk)) (Move (Square Fa rnk) (Square Fd rnk))
  where
    rnk =
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
