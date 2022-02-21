module Piece.KnightSpec where

import qualified Data.Set as Set
import Models.File
import Models.Move
import Models.Rank
import Models.Square
import Piece.Knight
import Test.Hspec

spec = do
  describe "emptyBoardMoves" $ do
    it "correctly calculate the knight jumps" $ do
      let
        start = Square Fa R1
        moves = getMoves (emptyBoardMoves N start)
        expected = map (Mv . Move start) [Square Fc R2, Square Fb R3]
      moves `shouldMatchList` expected

  -- describe "validMoves" $ do
  --   it "correctly filter out moves which end on a like piece" $ do
  --     let moves = validMoves (emptyBoardMoves N (Square Fa R1)) (Set.singleton (Square Fb R3))
  --     map (show . end) moves `shouldMatchList` ["c2"]