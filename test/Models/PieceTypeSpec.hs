module Models.PieceTypeSpec where

import Models.PieceType
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Read

instance Arbitrary PieceType where
  arbitrary = chooseEnum (minBound, maxBound)

spec = do
  describe "read" $ do
    prop "is inverse to show" $
      \x -> (read . show) x `shouldBe` (x :: PieceType)

    it "not read in anything else" $ do
      let result = readMaybe "s" :: Maybe PieceType
      result `shouldBe` Nothing
