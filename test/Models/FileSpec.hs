module Models.FileSpec where

import Models.File
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Read

instance Arbitrary File where
  arbitrary = chooseEnum (minBound, maxBound)

spec = do
  describe "read" $ do
    prop "is inverse to show" $
      \x -> (read . show) x `shouldBe` (x :: File)

    it "throw an error for anything else" $ do
      let result = readMaybe "wb" :: Maybe File
      result `shouldBe` Nothing
