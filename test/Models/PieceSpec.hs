module Models.PieceSpec where

import Models.ArbitraryInstances
import Models.Piece
import Models.PieceColour
import Models.PieceType
import Test.Hspec
import Test.Hspec.QuickCheck
import Text.Read

spec = do
  describe "read" $ do
    prop "is inverse to show" $
      \x -> (read . show) x `shouldBe` (x :: Piece)

    it "not read in anything else" $ do
      let result = readMaybe "Pp" :: Maybe Piece
      result `shouldBe` Nothing
