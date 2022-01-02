module Models.CastlingPrivileges where

import Text.Read
import Text.Regex.TDFA

data CastlingPrivileges = CastlingPrivileges
  { whiteKingSide, whiteQueenSide, blackKingSide, blackQueenSide :: Bool
  }
  deriving (Eq)

instance Show CastlingPrivileges where
  show (CastlingPrivileges wk wq bk bq) =
    case [c | (bool, c) <- zip [wk, wq, bk, bq] "KQkq", bool] of
      [] -> "-"
      s -> s

regexMatch :: String -> String
regexMatch s = s =~ "((K?Q?k?q)|-)"

-- instance Read CastlingPrivileges where
--   readPrec = do
--     Ident s <- lexP
--     case regexMatch s do 
--       "-" -> CastlingPrivileges False False False False 
      
--     return (CastlingPrivileges ('K' `elem` s) ('Q' `elem` s) ('k' `elem` s) ('q' `elem` s))
