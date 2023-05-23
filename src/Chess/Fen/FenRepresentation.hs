{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Chess.Fen.FenRepresentation where

import Control.Lens
import Data.Aeson
import qualified Data.Map as Map
import GHC.Generics
import Chess.Board
import Chess.Fen.CastlingPrivileges
import Chess.Piece
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
    pl <- arbitrary
    nextToMove <- arbitrary
    castlingPrivileges <- arbitrary
    enPassent <- arbitrary
    halfMoveClock <- arbitrary
    fullMoveClock <- arbitrary
    return $ FenRepresentation pl nextToMove castlingPrivileges enPassent halfMoveClock fullMoveClock
