module Models.PieceTypeSpec where

import Models.PieceType
import Test.Hspec
import Text.Read

spec = do
  describe "read" $ do
    it "correctly read in King" $ do
      let result = readMaybe "K"
      result `shouldBe` Just King

    it "correctly read in Queen" $ do
      let result = readMaybe "Q"
      result `shouldBe` Just Queen

    it "correctly read in Rook" $ do
      let result = readMaybe "R"
      result `shouldBe` Just Rook

    it "correctly read in Bishop" $ do
      let result = readMaybe "B"
      result `shouldBe` Just Bishop

    it "correctly read in Knight" $ do
      let result = readMaybe "N"
      result `shouldBe` Just Knight

    it "correctly read in Pawn" $ do
      let result = readMaybe "P"
      result `shouldBe` Just Pawn

    it "not read in anything else" $ do
      let result = readMaybe "s" :: Maybe PieceType
      result `shouldBe` Nothing
