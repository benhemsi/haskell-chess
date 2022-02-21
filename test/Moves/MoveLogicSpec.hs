module Moves.MoveLogicSpec where

import qualified Data.Set as Set
import Models.File
import Models.Move
import Models.Rank
import Models.Square
import Moves.MoveLogic
import Piece.Bishop
import Test.Hspec

spec = do
  describe "filterSlidingMoves" $ do
    it "correctly filter bishop sliding moves" $ do
      pending
      -- let
      --   start = Square Fa R1
      --   moves = emptyBoardMoves B start
      --   expected = map (Mv . Move start) $ [Square f r | f <- [Fb .. Fh] | r <- [R2 .. R8]]
      -- moves `shouldMatchList` expected

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
