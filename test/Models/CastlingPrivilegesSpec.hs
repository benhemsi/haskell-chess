{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Models.CastlingPrivilegesSpec where

import Models.CastlingPrivileges
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Read

makeBool :: Gen Bool
makeBool = chooseEnum (True, False)

instance Arbitrary CastlingPrivileges where
  arbitrary = do
    wk <- makeBool
    wq <- makeBool
    bk <- makeBool
    bq <- makeBool
    return (castlingPrivileges wk wq bk bq)

spec = do
  describe "read" $ do
    prop "is inverse to show" $
      \x -> (read . show) x `shouldBe` (x :: CastlingPrivileges)

    it "throw an error when the castling privileges are in the wrong order" $ do
      let result = readMaybe "kK" :: Maybe CastlingPrivileges
      result `shouldBe` Nothing

    it "throw an error for an incorrect string" $ do
      let result = readMaybe "a" :: Maybe CastlingPrivileges
      result `shouldBe` Nothing
