module Models.PieceColourSpec where

import Models.ArbitraryInstances
import Models.PieceColour
import Test.Hspec
import Test.Hspec.QuickCheck
import Text.Read

spec = do
  describe "read" $ do
    prop "is inverse to show" $ \x -> (read . show) x `shouldBe` (x :: PieceColour)
    it "throw an error for anything else" $ do
      let result = readMaybe "wb" :: Maybe PieceColour
      result `shouldBe` Nothing
