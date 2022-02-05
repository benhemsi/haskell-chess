{-# LANGUAGE QuasiQuotes #-}

module Models.CastlingPrivileges (CastlingPrivileges (..), getCastlingMoves, getKingSideCastle, getQueenSideCastle) where

import qualified Data.Set as Set
import Models.File
import Models.FullPieceList
import Models.Move
import Models.PieceColour
import Models.Rank
import Models.Square
import Text.RawString.QQ (r)
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

getKingSideCastle :: PieceColour -> Castle
getKingSideCastle colour = Castle (Move (Square Fe rank) (Square Fg rank)) (Move (Square Fh rank) (Square Ff rank))
  where
    rank = case colour of
      White -> R1
      Black -> R8

getQueenSideCastle :: PieceColour -> Castle
getQueenSideCastle colour = Castle (Move (Square Fe rank) (Square Fc rank)) (Move (Square Fa rank) (Square Fd rank))
  where
    rank = case colour of
      White -> R1
      Black -> R8

getCastlingMoves :: PieceColour -> FullPieceList -> CastlingPrivileges -> [Castle]
getCastlingMoves colour fullPL castlingPrivileges = output
  where
    (kingSidePrivilege, queenSidePrivilege, rank, attackedSquares) = case colour of
      White -> (whiteKingSide, whiteQueenSide, R1, blackAttackedSquares fullPL)
      Black -> (blackKingSide, blackQueenSide, R8, whiteAttackedSquares fullPL)
    kingSideSquaresToCheck = Set.unions [whiteOccupiedSquares fullPL, blackOccupiedSquares fullPL, attackedSquares]
    kingSide = kingSidePrivilege castlingPrivileges && (Square Ff rank `Set.notMember` kingSideSquaresToCheck) && (Square Fg rank `Set.notMember` kingSideSquaresToCheck)
    queenSideSquaresToCheck = Set.unions [whiteOccupiedSquares fullPL, blackOccupiedSquares fullPL, Set.delete (Square Fb rank) attackedSquares]
    queenSide = queenSidePrivilege castlingPrivileges && (Square Fb rank `Set.notMember` queenSideSquaresToCheck) && (Square Fc rank `Set.notMember` queenSideSquaresToCheck) && (Square Fd rank `Set.notMember` queenSideSquaresToCheck)
    start = Square Fe rank
    output = [getKingSideCastle colour | kingSide] ++ [getQueenSideCastle colour | queenSide]
