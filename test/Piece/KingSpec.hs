module Piece.KingSpec where

import qualified Data.Set as Set
import Models.File
import Models.Move
import Models.Rank
import Models.Square
import Piece.King
import Test.Hspec

spec = do
  describe "emptyBoardMoves" $ do
    it "correctly calculate King moves" $ do
      let 
        start = Square Fa R1
        moves = getMoves (emptyBoardMoves K start)
        expected  = map (Mv . Move start) [Square Fa R2, Square Fb R1, Square Fb R2]
      moves `shouldMatchList` expected

  -- describe "validMoves" $ do
  --   it "correctly filter out moves which end on a like piece" $ do
  --     let moves = validMoves (emptyBoardMoves K (Square Fa R1)) (Set.singleton (Square Fb R2))
  --     map (show . end) moves `shouldMatchList` ["a2", "b1"]
