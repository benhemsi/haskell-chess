{-# LANGUAGE QuasiQuotes #-}

module Models.CastlingPrivileges (CastlingPrivileges (..), castlingPrivileges) where

import Text.RawString.QQ (r)
import Text.Read
import Text.Regex.TDFA

data CastlingPrivileges = CastlingPrivileges
  { whiteKingSide, whiteQueenSide, blackKingSide, blackQueenSide :: Bool
  }
  deriving (Eq)

castlingPrivileges = CastlingPrivileges

instance Show CastlingPrivileges where
  show (CastlingPrivileges wk wq bk bq) =
    case [c | (bool, c) <- zip [wk, wq, bk, bq] "KQkq", bool] of
      [] -> "-"
      s -> s

regexMatch :: String -> Bool
regexMatch s = s =~ [r|^(-|(K?Q?k?q?))$|]

instance Read CastlingPrivileges where
  readPrec =
    ( do
        Symbol "-" <- lexP
        return (CastlingPrivileges False False False False)
    )
      +++ ( do
              Ident s <- lexP
              if regexMatch s
                then return (CastlingPrivileges ('K' `elem` s) ('Q' `elem` s) ('k' `elem` s) ('q' `elem` s))
                else pfail
          )

  readListPrec = readListPrecDefault
  readList = readListDefault
