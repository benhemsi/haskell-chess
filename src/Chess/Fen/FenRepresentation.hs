{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Chess.Fen.FenRepresentation where

import Chess.Board
import Chess.Fen.CastlingPrivileges
import Chess.Fen.EnPassentSquare
import Chess.Piece
import Control.Lens
import Data.Aeson
import qualified Data.Map as Map
import GHC.Generics
import Test.QuickCheck

data FenRepresentation =
  FenRepresentation
    { _pieces :: PieceList
    , _nextToMove :: PieceColour
    , _castlingPrivileges :: CastlingPrivileges
    , _enPassentSquare :: EnPassentSquare
    , _halfMoveClock, _fullMoveClock :: Int
    }
  deriving (Eq, Generic, Ord)

makeLenses ''FenRepresentation

instance ToJSON FenRepresentation

instance FromJSON FenRepresentation

buildBaseFenRepresentation :: PieceList -> FenRepresentation
buildBaseFenRepresentation pl = FenRepresentation pl White (CastlingPrivileges True True True True) (EnPSq Nothing) 0 1

startingFenRepresentation :: FenRepresentation
startingFenRepresentation = buildBaseFenRepresentation startingPieceList

instance Show FenRepresentation where
  show (FenRepresentation pieces nextToMove castlingPrivileges enPassent halfMoveClock fullMoveClock) =
    show pieces ++
    " " ++
    show nextToMove ++
    " " ++ show castlingPrivileges ++ " " ++ show enPassent ++ " " ++ show halfMoveClock ++ " " ++ show fullMoveClock

instance Arbitrary FenRepresentation where
  arbitrary = do
    int <- chooseInt (2, 32)
    pl <- arbitrary
    nextToMove <- arbitrary
    castlingPrivileges <- arbitrary
    enPassent <- arbitrary
    halfMoveClock <- arbitrary
    fullMoveClock <- arbitrary
    return $ FenRepresentation pl nextToMove castlingPrivileges enPassent halfMoveClock fullMoveClock
