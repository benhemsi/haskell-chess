module Chess.Moves.MoveTreeSpec where

import Chess.Board
import Chess.Fen
import Chess.Move
import Chess.Moves.MoveTree
import Chess.Piece
import Chess.Position
import Control.Lens
import Test.Hspec

spec = do
  describe "makeMove" $ do
    it "update all fields in Position when making a move" $ do
      let actual = makeMove (Mv $ Move (Square Fd R2) (Square Fd R4)) (buildBasePosition startingPieceList)
          expectedUpdates =
            [ over oppoPieces (changeKey (Square Fd R2) (Square Fd R4))
            , set (fen . nextToMove) Black
            , set (fen . enPassentSquare . enPassentSq) (Just $ Square Fd R3)
            ]
          expected = foldr (\f p -> f p) (buildBasePosition startingPieceList) expectedUpdates
      actual `shouldBe` expected
