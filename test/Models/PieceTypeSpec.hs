module Models.PieceTypeSpec where

import Models.Piece
import Test.Hspec
import Test.Hspec.QuickCheck
import Text.Read

spec = do
  describe "read" $ do
    prop "is inverse to show" $ \x -> (read . show) x `shouldBe` (x :: PieceType)
    it "not read in anything else" $ do
      let result = readMaybe "s" :: Maybe PieceType
      result `shouldBe` Nothing
