{-# LANGUAGE QuasiQuotes #-}

module FenParser where
import Models.Piece
import Models.PieceColour
import Models.PieceList
import Models.Square (Square)
import Moves
import Text.Regex
import Text.RawString.QQ (r)
import Control.Monad.Reader
import Text.Read
import Text.ParserCombinators.ReadPrec

data CastlingPrivileges = CastlingPrivileges
  { whiteKingSide, whiteQueenSide, blackKingSide, blackQueenSide :: Bool
  }

data FenRepresentation = FenRepresentation
  { pieceList :: PieceList,
    nextToMove :: PieceColour,
    castlingPrivileges :: CastlingPrivileges,
    enPassentSquare :: Maybe Square,
    halfMoveClock, fullMoveClock :: Int
  }

fenPattern = mkRegex [r|^\s*(([KQRBNPkqrbnp1-8]{1,8}+\/){7})([KQRBNPkqrbnp1-8]{1,8})\s+(w|b)\s+((K?Q?k?q?)|-)\s+([a-h][36]|-)\s+(0|[1-9]\d*)\s+(0|[1-9]\d*)$|]
