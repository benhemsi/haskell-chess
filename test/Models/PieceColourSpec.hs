module Models.PieceColourSpec where

import Models.PieceColour
import Test.Hspec
import Text.Read

spec = do
  describe "read" $ do
    it "correctly read in White" $ do
      let result = readMaybe "w"
      result `shouldBe` Just White

    it "correctly read in Black" $ do
      let result = readMaybe "b"
      result `shouldBe` Just Black

    it "throw an error for anything else" $ do
      let result = readMaybe "wb" :: Maybe PieceColour
      result `shouldBe` Nothing
