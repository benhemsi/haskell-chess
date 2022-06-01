{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.FenRepresentation where

import Control.Lens
import Models.CastlingPrivileges
import Models.Piece
import Models.PieceColour
import Models.PieceList
import Models.Square (Square)
import Text.ParserCombinators.ReadPrec
import Text.RawString.QQ (r)
import Text.Read

data FenRepresentation =
  FenRepresentation
    { _pieces :: PieceList
    , _nextToMove :: PieceColour
    , _castlingPrivileges :: CastlingPrivileges
    , _enPassentSquare :: Maybe Square
    , _halfMoveClock, _fullMoveClock :: Int
    }

makeLenses ''FenRepresentation

fenPattern =
  [r|^\s*(([KQRBNPkqrbnp1-8]{1,8}+\/){7})([KQRBNPkqrbnp1-8]{1,8})\s+(w|b)\s+((K?Q?k?q?)|-)\s+([a-h][36]|-)\s+(0|[1-9]\d*)\s+(0|[1-9]\d*)$|]

buildBaseFenRepresentation :: PieceList -> FenRepresentation
buildBaseFenRepresentation pl = FenRepresentation pl White (CastlingPrivileges True True True True) Nothing 0 1

startingFenRepresentation :: FenRepresentation
startingFenRepresentation = buildBaseFenRepresentation startingPieceList

instance Show FenRepresentation where
  show (FenRepresentation pieces nextToMove castlingPrivileges enPassent halfMoveClock fullMoveClock) =
    show pieces ++
    " " ++
    show nextToMove ++
    " " ++
    show castlingPrivileges ++ " " ++ showEnPassent enPassent ++ " " ++ show halfMoveClock ++ " " ++ show fullMoveClock
    where
      showEnPassent (Just sq) = show sq
      showEnPassent Nothing = "-"
