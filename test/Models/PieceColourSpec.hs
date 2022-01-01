module Models.PieceColourSpec where

import Models.PieceColour
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Read

instance Arbitrary PieceColour where
  arbitrary = chooseEnum (White, Black)

spec = do
  describe "read" $ do
    prop "is inverse to show" $
      \x -> (read . show) x `shouldBe` (x :: PieceColour)

    it "throw an error for anything else" $ do
      let result = readMaybe "wb" :: Maybe PieceColour
      result `shouldBe` Nothing
