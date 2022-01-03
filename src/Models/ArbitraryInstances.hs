{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Models.ArbitraryInstances where

import Models.CastlingPrivileges
import Models.File
import Models.Piece
import Models.PieceColour
import Models.PieceOnSquare hiding (piece)
import Models.PieceType
import Models.Rank
import Models.Square
import Test.QuickCheck

instance Arbitrary CastlingPrivileges where
  arbitrary = do
    wk <- arbitrary
    wq <- arbitrary
    bk <- arbitrary
    bq <- arbitrary
    return (castlingPrivileges wk wq bk bq)

instance Arbitrary File where
  arbitrary = chooseEnum (minBound, maxBound)

instance Arbitrary Piece where
  arbitrary =
    elements
      [ piece c t | c <- [White, Black], t <- [minBound .. maxBound]
      ]

instance Arbitrary PieceColour where
  arbitrary = chooseEnum (White, Black)

instance Arbitrary PieceType where
  arbitrary = chooseEnum (minBound, maxBound)

instance Arbitrary Rank where
  arbitrary = chooseEnum (minBound, maxBound)

instance Arbitrary Square where
  arbitrary = chooseEnum (minBound, maxBound)

instance Arbitrary PieceOnSquare where
  arbitrary = do
    p <- arbitrary
    sq <- arbitrary
    return (pieceOnSquare p sq)
