module Models.Fen.FenRepresentationSpec where

import Models.Fen
import Test.Hspec

spec = do
  describe "show" $ do
    it "correctly show the starting FEN string" $ do
      let fen = startingFenRepresentation
          actual = show fen
      actual `shouldBe` "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
