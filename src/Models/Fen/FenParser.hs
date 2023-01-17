{-# LANGUAGE QuasiQuotes #-}

module Models.Fen.FenParser where

import Data.Char
import qualified Data.List.NonEmpty as NE
import Data.List.Split
import Data.Validation
import Models.Piece
import Text.RawString.QQ (r)
import Text.Regex.TDFA

fenPattern :: String
fenPattern =
  [r|^\s*(([KQRBNPkqrbnp1-8]{1,8}+\/){7})([KQRBNPkqrbnp1-8]{1,8})\s+(w|b)\s+((K?Q?k?q?)|-)\s+([a-h][36]|-)\s+(0|[1-9]\d*)\s+(0|[1-9]\d*)$|]

validateWhiteSpace :: String -> Validation FenError [String]
validateWhiteSpace s =
  let splitString = words s
      len = length splitString
   in if len == 6
        then Success splitString
        else Failure (IncorrectFieldNumber len)

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
      validateSquareNumber = toEither . foldr1 (<>) . map checkNumberOfSquares
      regexToken = "[KQRBNPkqrbnp1-8]"
      regex = [r|^(([KQRBNPkqrbnp1-8]{1,8}\/){7})([KQRBNPkqrbnp1-8]{1,8})$|]
      regexEither :: Either FenError [PieceOnSquare]
      regexEither =
        if s =~ regex
          then Right $ (initialPass' . secondaryPass') s
          else Left (InvalidPieceListCharacters "foo")
      outputEither = do
        validatedRows <- eitherSplitString
        validatedSquares <- validateSquareNumber validatedRows
        regexEither
   in fromEither outputEither

-- parseFen :: String -> Validation FenError FenRepresentation
-- parseFen s = 
data FenError
  = IncorrectFieldNumber Int
  | IncorrectRowNumber Int
  | IncorrectSquaresPerRow Int
  | InvalidPieceListCharacters String
  | CombinedFenError (NE.NonEmpty FenError)
  deriving (Eq, Show)

instance Semigroup FenError where
  CombinedFenError xs <> CombinedFenError ys = CombinedFenError (xs <> ys)
  CombinedFenError xs <> y = CombinedFenError (xs <> (y NE.:| []))
  x <> CombinedFenError ys = CombinedFenError (x NE.<| ys)
  x <> y = CombinedFenError (x NE.:| [y])
