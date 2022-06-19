{-# LANGUAGE ParallelListComp #-}

module Piece.QueenSpec where

import Models.Board
import Models.Move
import Moves.MoveFiltering
import Piece.Queen
import Test.Hspec

spec = do
  describe "emptyBoardMoves" $ do
    it "correctly combine rook and bishop moves" $ do
      let start = Square Fa R1
          moves = flattenMoves (emptyBoardMoves Q start)
          expected =
            map (Mv . Move start) $
            [Square f r | f <- [Fb .. Fh] | r <- [R2 .. R8]] ++
            [Square f R1 | f <- [Fb .. Fh]] ++ [Square Fa r | r <- [R2 .. R8]]
      moves `shouldMatchList` expected
  -- describe "validMoves" $ do
  --   it "correctly combine rook and bishop moves" $ do
  --     let moves = validMoves (emptyBoardMoves (Square Fa R1)) (Set.fromList [Square Fc R3, Square Fa R3, Square Fc R1]) Set.empty
  --     map (show . end) moves `shouldMatchList` ["a2", "b1", "b2"]
