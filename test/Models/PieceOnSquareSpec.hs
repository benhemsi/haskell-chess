module Models.PieceOnSquareSpec where

import Models.Piece
import Models.PieceColour
import Models.PieceOnSquare
import Models.PieceType
import Models.Square
import Test.Hspec
import Test.Hspec.QuickCheck
import Text.Read

spec = do
  describe "read" $ do
    prop "is inverse to show" $ \x -> (read . show) x `shouldBe` (x :: PieceOnSquare)
    it "not read in anything else" $ do
      let result = readMaybe "Pp" :: Maybe PieceOnSquare
      result `shouldBe` Nothing
  describe "showList" $ do
    it "initialPass should work" $ do
      initialPass [PieceOnSquare (Piece White King) (Square Fa R1)] `shouldBe` 'K' : ['1' | _ <- [1 .. 63]]
    it "secondaryPass should work" $ do secondaryPass ('K' : ['1' | _ <- [1 .. 63]]) `shouldBe` "8/8/8/8/8/8/8/K7"
    it "should work" $ do showList [PieceOnSquare (Piece White King) (Square Fa R1)] "" `shouldBe` "8/8/8/8/8/8/8/K7"
  describe "readList" $ do
    it "secondaryPass' should work" $ do secondaryPass' "8/8/8/8/8/8/8/K7" `shouldBe` 'K' : ['1' | _ <- [1 .. 63]]
    it "initialPass' should work" $ do
      initialPass' ('K' : ['1' | _ <- [1 .. 63]]) `shouldBe` [PieceOnSquare (Piece White King) (Square Fa R1)]
