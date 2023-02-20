{-# LANGUAGE QuasiQuotes #-}

module Models.Fen.FenParser where

import Data.Char
import Data.Foldable
import qualified Data.List.NonEmpty as NE
import Data.List.Split
import qualified Data.Set as Set
import Data.Validation
import Models.Board
import Models.Fen.CastlingPrivileges
import Models.Fen.FenError
import Models.Fen.FenRepresentation
import Models.Piece
import Models.Piece.PieceList
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
  let splitString = splitOn "/" s
      len = length splitString
      eitherSplitString =
        if len == 8
          then Right splitString
          else Left (IncorrectRowNumber len)
      convertCharToNum c =
        if isDigit c
          then digitToInt c
          else 1
      checkNumberOfSquares str =
        let squareN = sum $ map convertCharToNum str
         in if squareN == 8
              then Success str
              else Failure $ IncorrectSquaresPerRow squareN
      validateSquareNumber = toEither . foldr1 (<*) . map checkNumberOfSquares
      validCharacters = Set.fromList "KQRBNPkqrbnp12345678/"
      regex = [r|^(([KQRBNPkqrbnp1-8]{1,8}\/){7})([KQRBNPkqrbnp1-8]{1,8})$|]
      regexEither :: Either FenError PieceList
      regexEither =
        if s =~ regex
          then Right $ (initialPass' . secondaryPass') s
          else Left (InvalidPieceListCharacters $ filter (`Set.notMember` validCharacters) s)
      outputEither = do
        validatedRows <- eitherSplitString
        _ <- validateSquareNumber validatedRows
        regexEither
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

parseEnPassentSquare :: String -> Validation FenError (Maybe Square)
parseEnPassentSquare ['-'] = Success Nothing
parseEnPassentSquare s = fmap Just (parseUsingRead InvalidEnPassentSquare s)

parseMoveClock :: String -> Validation FenError Int
parseMoveClock s =
  case readMaybe s of
    Nothing -> Failure (InvalidMoveClock s)
    Just x ->
      if x < 0
        then Failure (InvalidMoveClock s)
        else Success x
