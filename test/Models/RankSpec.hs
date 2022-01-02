module Models.RankSpec where

import Models.Rank
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Read

instance Arbitrary Rank where
  arbitrary = chooseEnum (minBound, maxBound)

spec = do
  describe "read" $ do
    prop "is inverse to show" $
      \x -> (read . show) x `shouldBe` (x :: Rank)

    it "throw an error for anything else" $ do
      let result = readMaybe "wb" :: Maybe Rank
      result `shouldBe` Nothing
