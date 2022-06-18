module Models.RankSpec where

import Models.Square
import Test.Hspec
import Test.Hspec.QuickCheck
import Text.Read

spec = do
  describe "read" $ do
    prop "is inverse to show" $ \x -> (read . show) x `shouldBe` (x :: Rank)
    it "throw an error for anything else" $ do
      let result = readMaybe "wb" :: Maybe Rank
      result `shouldBe` Nothing
