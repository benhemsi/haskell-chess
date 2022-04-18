module Models.SquareSpec where

import Models.Square
import Test.Hspec
import Test.Hspec.QuickCheck
import Text.Read

spec = do
  describe "read" $ do
    prop "is inverse to show" $ \x -> (read . show) x `shouldBe` (x :: Square)
    it "throw an error for anything else" $ do
      let result = readMaybe "wb" :: Maybe Square
      result `shouldBe` Nothing
