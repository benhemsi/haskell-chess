{-# LANGUAGE ParallelListComp #-}

module Piece.BishopSpec where

import Chess.Board
import Chess.Move
import Chess.Moves.MoveFiltering
import Piece.Bishop
import Test.Hspec

spec = do
  describe "emptyBoardMoves" $ do
    it "correctly calculate the northEast diagonal" $ do
      let start = Square Fa R1
          moves = flattenMoves (emptyBoardMoves B start)
          expected = map (Mv . Move start) $ [Square f r | f <- [Fb .. Fh] | r <- [R2 .. R8]]
      moves `shouldMatchList` expected
    it "correctly calculate the southEast diagonal" $ do
      let start = Square Fa R8
          moves = flattenMoves (emptyBoardMoves B start)
          expected = map (Mv . Move start) $ [Square f r | f <- [Fb .. Fh] | r <- reverse [R1 .. R7]]
      moves `shouldMatchList` expected
    it "correctly calculate the southWest diagonal" $ do
      let start = Square Fh R8
          moves = flattenMoves (emptyBoardMoves B start)
          expected = map (Mv . Move start) $ [Square f r | f <- [Fa .. Fg] | r <- [R1 .. R7]]
      moves `shouldMatchList` expected
    it "correctly calculate the northWest diagonal" $ do
      let start = Square Fh R1
          moves = flattenMoves (emptyBoardMoves B start)
          expected = map (Mv . Move start) $ [Square f r | f <- reverse [Fa .. Fg] | r <- [R2 .. R8]]
      moves `shouldMatchList` expected
  -- describe "validMoves" $ do
  --   it "correctly filter up to but not including a like piece on the same diagonal" $ do
  --     let moves = validMoves (emptyBoardMoves B (Square Fa R1)) (Set.singleton (Square Fc R3)) Set.empty
  --     map (show . attackedSquare) moves `shouldMatchList` ["b2"]
  --   it "correctly filter up to and including an oppo piece on the same diagonal" $ do
  --     let moves = validMoves (emptyBoardMoves B (Square Fa R1)) Set.empty (Set.singleton (Square Fc R3))
  --     map (show . attackedSquare) moves `shouldMatchList` ["b2", "c3"]
  --   it "correctly filter when there are multiple pieces on the same diagonal" $ do
  --     let moves = validMoves (emptyBoardMoves B (Square Fa R1)) (Set.fromList [Square Fe R5, Square Ff R6]) (Set.fromList [Square Fc R3, Square Fd R4])
  --     map (show . attackedSquare) moves `shouldMatchList` ["b2", "c3"]
