module Chess.Piece.PieceOnSquareSpec where

import qualified Data.Map as Map
import Chess.Board
import Chess.Piece
import Test.Hspec
import Test.Hspec.QuickCheck
import Text.Read

spec = do
  describe "read" $ do
    prop "is inverse to show" $ \x -> (read . show) x `shouldBe` (x :: PieceOnSquare)
    it "not read in anything else" $ do
      let result = readMaybe "Pp" :: Maybe PieceOnSquare
      result `shouldBe` Nothing
