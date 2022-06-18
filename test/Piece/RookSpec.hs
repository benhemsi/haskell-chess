module Piece.RookSpec where

import Models.Move
import Models.Square
import Moves.MoveFiltering
import Piece.Rook
import Test.Hspec

spec = do
  describe "emptyBoardMoves" $ do
    it "correctly calculate the north and east moves" $ do
      let start = Square Fa R1
          moves = flattenMoves (emptyBoardMoves R start)
          expected = map (Mv . Move start) $ [Square f R1 | f <- [Fb .. Fh]] ++ [Square Fa r | r <- [R2 .. R8]]
      moves `shouldMatchList` expected
    it "correctly calculate the south and west moves" $ do
      let start = Square Fh R8
          moves = flattenMoves (emptyBoardMoves R start)
          expected = map (Mv . Move start) $ [Square f R8 | f <- [Fa .. Fg]] ++ [Square Fh r | r <- [R1 .. R7]]
      moves `shouldMatchList` expected
  -- describe "validMoves" $ do
  --   it "correctly filter up to but not including a like piece on the same rank/file" $ do
  --     let moves = validMoves (emptyBoardMoves (Square Fa R1)) (Set.fromList [Square Fa R3, Square Fc R1]) Set.empty
  --     map (show . end) moves `shouldMatchList` ["a2", "b1"]
  --   it "correctly filter up to and including an oppo piece on the same rank/file" $ do
  --     let moves = validMoves (emptyBoardMoves (Square Fa R1)) Set.empty (Set.fromList [Square Fa R3, Square Fc R1])
  --     map (show . end) moves `shouldMatchList` ["a2", "a3", "b1", "c1"]
