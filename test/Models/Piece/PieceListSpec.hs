module Models.Piece.PieceListSpec where

import Models.Piece.PieceList
import Test.Hspec
import Test.Hspec.QuickCheck

spec = do
  describe "show" $ do
    it "correctly show the starting piece list" $ do
      let actual = show startingPieceList
      actual `shouldBe` "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
  -- describe "read" $ do
  --   it "correctly read the starting piece list" $ do
  --     let actual = read "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
  --     actual `shouldBe` startingPieceList
  --   it "correctly read the starting piece list" $ do
  --     let actual = initialPass' $ secondaryPass' "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
  --     actual `shouldBe` startingPieceList
  -- describe "read" $ do prop "is inverse to show" $ \x -> (read . show) x `shouldBe` (x :: PieceList)
