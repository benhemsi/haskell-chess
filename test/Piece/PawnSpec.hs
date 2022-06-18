module Piece.PawnSpec where

import Models.File
import Models.Move
import Models.Rank
import Models.Square
import Moves.MoveFiltering
import Piece.Pawn
import Test.Hspec

spec = do
  describe "emptyBoardMoves" $ do
    it "correctly calculate the white and black pawn moves" $ do
      let start = Square Fb R2
          moves = flattenMoves $ emptyBoardMoves P start
          expectedWhite = map (Mv . Move start) [Square Fa R3, Square Fb R3, Square Fb R4, Square Fc R3]
          expectedBlack = map (PP . PawnPromotion . Move start) [Square Fa R1, Square Fb R1, Square Fc R1]
      moves `shouldMatchList` expectedWhite ++ expectedBlack
    -- it "correctly calculate the black pawn moves" $ do
    --   let moves = emptyBoardMoves P (Square Fb R2)
    --   map (show . end) (black moves) `shouldMatchList` ["a1", "b1", "c1"]
    -- it "return no moves if on the first rank" $ do
    --   let moves = emptyBoardMoves P (Square Fb R1)
    --   white moves ++ black moves `shouldMatchList` []
    -- it "return no moves if on the eigth rank" $ do
    --   let moves = emptyBoardMoves P (Square Fb R8)
    --   white moves ++ black moves `shouldMatchList` []
  -- describe "validMoves" $ do
    -- it "correctly filter out forward moves when a like piece is in front" $ do
    --   let moves = white (validMoves (emptyBoardMoves P (Square Fb R2)) (Set.singleton (Square Fb R3)) Set.empty)
    --   map (show . end) moves `shouldMatchList` []
    -- it "correctly filter out forward moves when an oppo piece is in front" $ do
    --   let moves = white (validMoves (emptyBoardMoves P (Square Fb R2)) Set.empty (Set.singleton (Square Fc R3)))
    --   map (show . end) moves `shouldMatchList` ["b3", "c3"]
