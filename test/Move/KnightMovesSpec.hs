module Move.KnightMovesSpec where

import qualified Data.Set as Set
import Models.File
import Models.Move
import Models.Rank
import Models.Square
import Move.KnightMoves
import Test.Hspec

spec = do
  describe "emptyBoardMoves" $ do
    it "correctly calculate the knight jumps" $ do
      let moves = emptyBoardMoves (Square Fa R1)
      map (show . end) moves `shouldMatchList` ["c2", "b3"]

  describe "validMoves" $ do
    it "correctly filter out moves which end on a like piece" $ do
      let moves = validMoves (emptyBoardMoves (Square Fa R1)) (Set.singleton (Square Fb R3))
      map (show . end) moves `shouldMatchList` ["c2"]
