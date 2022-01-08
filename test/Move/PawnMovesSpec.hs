module Move.PawnMovesSpec where

import qualified Data.Set as Set
import Models.File
import Models.Move
import Models.Rank
import Models.Square
import Move.PawnMoves
import Test.Hspec

spec = do
  describe "emptyBoardMoves" $ do
    it "correctly calculate the white pawn moves" $ do
      let moves = emptyBoardMoves (Square Fb R2)
      map (show . end) (white moves) `shouldMatchList` ["a3", "b3", "c3"]

    it "correctly calculate the black pawn moves" $ do
      let moves = emptyBoardMoves (Square Fb R2)
      map (show . end) (black moves) `shouldMatchList` ["a1", "b1", "c1"]

    it "return no moves if on the first rank" $ do
      let moves = emptyBoardMoves (Square Fb R1)
      white moves ++ black moves `shouldMatchList` []

    it "return no moves if on the eigth rank" $ do
      let moves = emptyBoardMoves (Square Fb R8)
      white moves ++ black moves `shouldMatchList` []

  describe "validMoves" $ do
    it "correctly filter out forward moves when a like piece is in front" $ do
      let moves = white (validMoves (emptyBoardMoves (Square Fb R2)) (Set.singleton (Square Fb R3)) Set.empty)
      map (show . end) moves `shouldMatchList` []

    it "correctly filter out forward moves when an oppo piece is in front" $ do
      let moves = white (validMoves (emptyBoardMoves (Square Fb R2)) Set.empty (Set.singleton (Square Fc R3)))
      map (show . end) moves `shouldMatchList` ["b3", "c3"]
