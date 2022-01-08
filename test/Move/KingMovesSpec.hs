module Move.KingMovesSpec where

import qualified Data.Set as Set
import Models.File
import Models.Move
import Models.Rank
import Models.Square
import Move.KingMoves
import Test.Hspec

spec = do
  describe "emptyBoardMoves" $ do
    it "correctly calculate King moves" $ do
      let moves = emptyBoardMoves (Square Fa R1)
      map (show . end) moves `shouldMatchList` ["a2", "b1", "b2"]

  describe "validMoves" $ do
    it "correctly filter out moves which end on a like piece" $ do
      let moves = validMoves (emptyBoardMoves (Square Fa R1)) (Set.singleton (Square Fb R2))
      map (show . end) moves `shouldMatchList` ["a2", "b1"]
