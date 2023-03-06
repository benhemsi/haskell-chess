module Moves.MoveTreeSpec where

import Models.Board
import Models.Move
import Models.Piece
import Models.Position (buildBasePosition)
import Moves.MoveTree
import Test.Hspec

spec = do
  describe "makeMove" $ do
    it "update all fields in Position when making a move" $ do
      let actual = makeMove (Mv $ Move (Square Fd R2) (Square Fd R4)) (buildBasePosition startingPieceList)
          expected = buildBasePosition (changeKey (Square Fd R2) (Square Fd R4) startingPieceList)
      actual `shouldBe` expected
