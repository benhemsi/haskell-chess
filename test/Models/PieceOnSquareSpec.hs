module Models.PieceOnSquareSpec where

import Models.ArbitraryInstances
import Models.PieceOnSquare
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Gen
import Text.Read

spec = do
  describe "read" $ do
    prop "is inverse to show" $ \x ->
      (read . show) x `shouldBe` (x :: PieceOnSquare)
    it "not read in anything else" $ do
      let result = readMaybe "Pp" :: Maybe PieceOnSquare
      result `shouldBe` Nothing
  describe "readList" $ do
    it "should read in a list" $
      -- readList "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR" `shouldBe` ([] :: [([PieceOnSquare], String)])
      pending
