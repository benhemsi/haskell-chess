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
import Models.Fen.FenRepresentation
import Models.Piece
import Text.RawString.QQ (r)
import Text.Read
import Text.Regex.TDFA

vp :: Semigroup e => Validation e a -> Validation e b -> Validation e (a, b)
Failure e1 `vp` Failure e2 = Failure (e1 <> e2)
Failure e1 `vp` _ = Failure e1
_ `vp` Failure e2 = Failure e2
Success x `vp` Success y = Success (x, y)

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
      combined = parsedPl `vp` parsedN2mv `vp` parsedCst `vp` parsedEnp `vp` parsedHalf `vp` parsedFull
  (((((vpl, vn2mv), vcst), venp), vhalf), vfull) <- toEither combined
  return $ FenRepresentation vpl vn2mv vcst venp vhalf vfull

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
      regexEither :: Either FenError [PieceOnSquare]
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

data FenError
  = IncorrectFieldNumber Int
  | IncorrectRowNumber Int
  | IncorrectSquaresPerRow Int
  | InvalidPieceListCharacters String
  | InvalidNextToMove String
  | InvalidCastlingPrivileges String
  | InvalidEnPassentSquare String
  | InvalidMoveClock String
  | CombinedFenError (NE.NonEmpty FenError)
  deriving (Eq)

instance Semigroup FenError where
  CombinedFenError xs <> CombinedFenError ys = CombinedFenError (xs <> ys)
  CombinedFenError xs <> y = CombinedFenError (xs <> (y NE.:| []))
  x <> CombinedFenError ys = CombinedFenError (x NE.<| ys)
  x <> y = CombinedFenError (x NE.:| [y])

instance Show FenError where
  show (IncorrectFieldNumber i) =
    "Incorrect number of fields. A valid FEN string must have 6 fields separated by whitespace. The provided string had " ++
    show i ++ " fields."
  show (IncorrectRowNumber i) =
    "Incorrect number of rows. A valid board must have 8 rows separated by '/'. The provided string had " ++
    show i ++ " rows."
  show (IncorrectSquaresPerRow i) =
    "Incorrect number of squares. A valid board must have 8 squares per row. The provided string had " ++
    show i ++ " squares in one of the rows."
  show (InvalidPieceListCharacters s) =
    "Invalid piece list characters are present. All characters must be one of the following 'KQRBNPkqrbnp12345678/'. " ++
    "The provided string had the following characters which are invalid '" ++ s ++ "'."
  show (InvalidNextToMove s) =
    "Invalid next to move. The next to move must be 'w' or 'b'. The provided string '" ++ s ++ "' is invalid."
  show (InvalidCastlingPrivileges s) =
    "Invalid castling privileges. The castling privileges must only contain 'KQkq' or be set to '-'. The provided string '" ++
    s ++ "' is invalid."
  show (InvalidEnPassentSquare s) =
    "Invalid en passent square. The en passent square must be 'a3', 'a6', 'h3', 'h6' or '-'. The provided string '" ++
    s ++ "' is invalid."
  show (InvalidMoveClock s) =
    "Invalid move clock. The move clock must be a positive integer. The provided string '" ++ s ++ "' is invalid."
  show (CombinedFenError errors) = foldl' (\curr err -> curr ++ "\n" ++ show err) "" (toList errors)
