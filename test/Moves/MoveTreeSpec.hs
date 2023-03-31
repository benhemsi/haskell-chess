module Moves.MoveTreeSpec where

import Control.Lens
import Models.Board
import Models.Fen
import Models.Move
import Models.Piece
import Models.Position
import Moves.MoveTree
import Test.Hspec

spec = do
  describe "makeMove" $ do
    it "update all fields in Position when making a move" $ do
      let actual = makeMove (Mv $ Move (Square Fd R2) (Square Fd R4)) (buildBasePosition startingPieceList)
          expectedUpdates =
            [ over oppoPieces (changeKey (Square Fd R2) (Square Fd R4))
            , set (fen . nextToMove) Black
            , set (fen . enPassentSquare) (Just $ Square Fd R3)
            ]
          expected = foldr (\f p -> f p) (buildBasePosition startingPieceList) expectedUpdates
      actual `shouldBe` expected
