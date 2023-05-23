module Chess.Fen.FenError where

import Data.Foldable
import qualified Data.List.NonEmpty as NE

data FenError
  = IncorrectFieldNumber Int
  | IncorrectRowNumber Int
  | IncorrectSquaresPerRow Int
  | InvalidPieceListCharacters String
  | InvalidNextToMove String
  | InvalidCastlingPrivileges String
  | InvalidEnPassentSquare String
  | InvalidMoveClock String
  | IncorrectNumberOfKings Int Int
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
  show (IncorrectNumberOfKings whiteN blackN) =
    "Incorrect number of kings. There must be one white king and one black king. There was " ++
    show whiteN ++ " white kings and " ++ show blackN ++ " black kings."
  show (CombinedFenError errors) = foldl' (\curr err -> curr ++ "\n" ++ show err) "" (toList errors)
