{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

{-# HLINT ignore "Use <$>" #-}
module Models.Piece.PieceOnSquare where

import Control.Lens
import Data.Aeson
import Data.List (intercalate, sort)
import Data.List.Split (chunksOf, splitOn)
import GHC.Generics
import Models.Board
import Models.Piece.Piece
import Models.Piece.PieceList
import Test.QuickCheck
import Text.Read

data PieceOnSquare =
  PieceOnSquare
    { _piece :: Piece
    , _square :: Square
    }
  deriving (Eq, Generic)

makeLenses ''PieceOnSquare

instance ToJSON PieceOnSquare

instance FromJSON PieceOnSquare

instance Ord PieceOnSquare where
  a <= b = _square a <= _square b

instance Show PieceOnSquare where
  show p = (show . _piece) p ++ (show . _square) p

instance Read PieceOnSquare where
  readPrec = do
    Ident (f:r) <- lexP
    let pce = readMaybe [f]
        sq = readMaybe r
    case (pce, sq) of
      (Just p, Just sq) -> return (PieceOnSquare p sq)
      _ -> pfail

instance Arbitrary PieceOnSquare where
  arbitrary = do
    p <- arbitrary
    sq <- arbitrary
    return (PieceOnSquare p sq)
