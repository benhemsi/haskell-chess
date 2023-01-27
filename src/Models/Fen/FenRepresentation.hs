{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.Fen.FenRepresentation where

import Control.Lens
import Data.Aeson
import GHC.Generics
import Models.Board
import Models.Fen.CastlingPrivileges
import Models.Piece
import Test.QuickCheck

data FenRepresentation =
  FenRepresentation
    { _pieces :: PieceList
    , _nextToMove :: PieceColour
    , _castlingPrivileges :: CastlingPrivileges
    , _enPassentSquare :: Maybe Square
    , _halfMoveClock, _fullMoveClock :: Int
    }
  deriving (Eq, Generic)

makeLenses ''FenRepresentation

instance ToJSON FenRepresentation

instance FromJSON FenRepresentation

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

instance Arbitrary FenRepresentation where
  arbitrary = do
    int <- chooseInt (2, 32)
    pieces <- vector int
    nextToMove <- arbitrary
    castlingPrivileges <- arbitrary
    enPassent <- arbitrary
    halfMoveClock <- arbitrary
    fullMoveClock <- arbitrary
    return $ FenRepresentation pieces nextToMove castlingPrivileges enPassent halfMoveClock fullMoveClock
