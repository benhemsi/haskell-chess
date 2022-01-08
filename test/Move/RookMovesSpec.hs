module Move.RookMovesSpec where

import qualified Data.Set as Set
import Models.File
import Models.Move
import Models.Rank
import Models.Square
import Move.RookMoves
import Test.Hspec

spec = do
  describe "emptyBoardMoves" $ do
    it "correctly calculate the north and east moves" $ do
      let moves = flattenMoves (emptyBoardMoves (Square Fa R1))
      map (show . end) moves `shouldMatchList` map (("a" ++) . show) [2 .. 8] ++ map (: "1") ['b' .. 'h']

    it "correctly calculate the south and west moves" $ do
      let moves = flattenMoves (emptyBoardMoves (Square Fh R8))
      map (show . end) moves `shouldMatchList` map (("h" ++) . show) [1 .. 7] ++ map (: "8") ['a' .. 'g']

  describe "validMoves" $ do
    it "correctly filter up to but not including a like piece on the same rank/file" $ do
      let moves = validMoves (emptyBoardMoves (Square Fa R1)) (Set.fromList [Square Fa R3, Square Fc R1]) Set.empty
      map (show . end) moves `shouldMatchList` ["a2", "b1"]

    it "correctly filter up to and including an oppo piece on the same rank/file" $ do
      let moves = validMoves (emptyBoardMoves (Square Fa R1)) Set.empty (Set.fromList [Square Fa R3, Square Fc R1])
      map (show . end) moves `shouldMatchList` ["a2", "a3", "b1", "c1"]
