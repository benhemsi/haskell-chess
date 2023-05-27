{-# LANGUAGE QuasiQuotes #-}

module Chess.Fen.FenParser where

import Chess.Board
import Chess.Fen.CastlingPrivileges
import Chess.Fen.EnPassentSquare
import Chess.Fen.FenError
import Chess.Fen.FenRepresentation
import Chess.Piece
import Chess.Piece.PieceList
import Data.Char
import Data.Foldable
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import Data.List.Split
import qualified Data.Set as Set
import Data.Validation
import Text.RawString.QQ (r)
import Text.Read
import Text.Regex.TDFA

parseFen :: String -> Either FenError FenRepresentation
parseFen s = do
  splitString <- validateWhiteSpace s
  let [pl, n2mv, cst, enp, half, full] = splitString
      parsedPl = parsePieces pl
      parsedN2mv = parseNextToMove n2mv
      parsedCst = parseCastlingPrivileges cst
      parsedEnp = parseEnPassentSquare enp
      parsedHalf = parseMoveClock half
      parsedFull = parseMoveClock full
      combined = FenRepresentation <$> parsedPl <*> parsedN2mv <*> parsedCst <*> parsedEnp <*> parsedHalf <*> parsedFull
  toEither combined

validateWhiteSpace :: String -> Either FenError [String]
validateWhiteSpace s =
  let splitString = words s
      len = length splitString
   in if len == 6
        then Right splitString
        else Left (IncorrectFieldNumber len)

parsePieces :: String -> Validation FenError PieceList
parsePieces s =
  let checkNumberOfRows str =
        if len == 8
          then Right splitString
          else Left (IncorrectRowNumber len)
        where
          splitString = splitOn "/" str
          len = length splitString
      convertCharToNum c =
        if isDigit c
          then digitToInt c
          else 1
      checkNumberOfSquares str =
        let squareN = sum $ map convertCharToNum str
         in if squareN == 8
              then Success str
              else Failure $ IncorrectSquaresPerRow squareN
      validateSquareNumber = toEither . fmap (intercalate "/") . traverse checkNumberOfSquares
      validCharacters = Set.fromList "KQRBNPkqrbnp12345678/"
      regex = [r|^(([KQRBNPkqrbnp1-8]{1,8}\/){7})([KQRBNPkqrbnp1-8]{1,8})$|]
      checkRegex str =
        if str =~ regex
          then Right str
          else Left (InvalidPieceListCharacters $ filter (`Set.notMember` validCharacters) str)
      checkKingNumber str =
        let whiteKingN = length $ filter (== 'K') str
            blackKingN = length $ filter (== 'k') str
            out =
              if whiteKingN /= 1 || blackKingN /= 1
                then Left (IncorrectNumberOfKings whiteKingN blackKingN)
                else Right str
         in out
      outputEither = do
        validatedRows <- checkNumberOfRows s
        validatedSquareN <- validateSquareNumber validatedRows
        validatedRegex <- checkRegex validatedSquareN
        validatedKingN <- checkKingNumber validatedRegex
        return $ (initialPass' . secondaryPass') validatedKingN
   in fromEither outputEither

parseUsingRead :: Read a => (String -> FenError) -> String -> Validation FenError a
parseUsingRead error s =
  case readMaybe s of
    Nothing -> Failure (error s)
    Just x -> Success x

parseNextToMove :: String -> Validation FenError PieceColour
parseNextToMove = parseUsingRead InvalidNextToMove

parseCastlingPrivileges :: String -> Validation FenError CastlingPrivileges
parseCastlingPrivileges = parseUsingRead InvalidCastlingPrivileges

parseEnPassentSquare :: String -> Validation FenError EnPassentSquare
parseEnPassentSquare = parseUsingRead InvalidEnPassentSquare

parseMoveClock :: String -> Validation FenError Int
parseMoveClock s =
  case readMaybe s of
    Nothing -> Failure (InvalidMoveClock s)
    Just x ->
      if x < 0
        then Failure (InvalidMoveClock s)
        else Success x
