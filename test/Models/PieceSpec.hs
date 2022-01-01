module Models.PieceSpec where

import Models.Piece
import Models.PieceColour
import Models.PieceType
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Read

instance Arbitrary Piece where
  arbitrary =
    elements
      [ piece c t | c <- [White, Black], t <- [minBound .. maxBound]
      ]

spec = do
  describe "read" $ do
    prop "is inverse to show" $
      \x -> (read . show) x `shouldBe` (x :: Piece)

    it "not read in anything else" $ do
      let result = readMaybe "Pp" :: Maybe Piece
      result `shouldBe` Nothing
