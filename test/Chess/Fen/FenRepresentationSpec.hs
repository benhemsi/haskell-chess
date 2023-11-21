module Chess.Fen.FenRepresentationSpec where

import Chess.Fen
import Chess.Piece
import Control.Lens
import qualified Data.Map as Map
import Test.Hspec

fen = startingFenRepresentation

spec = do
  describe "show" $ do
    it "correctly show the starting FEN string" $ do
      let fen = startingFenRepresentation
          actual = show fen
      actual `shouldBe` "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  describe "likePieces" $ do
    it "fetch like pieces" $ do view likePieces fen `shouldBe` fen ^. pieces . whitePieces
    it "obey the view set law" $ do view likePieces (set likePieces Map.empty fen) `shouldBe` Map.empty
    it "obey the set view law" $ do set likePieces (view likePieces fen) fen `shouldBe` fen
    it "obey the set set law" $ do
      set likePieces Map.empty (set likePieces (startingPieceList ^. whitePieces) fen) `shouldBe`
        set likePieces Map.empty fen
  describe "oppoPieces" $ do it "fetch oppo pieces" $ do view oppoPieces fen `shouldBe` fen ^. pieces . blackPieces
  describe "switchNextToMove" $ do
    it "work with the lenses" $ do
      view likePieces fen `shouldBe` view oppoPieces (switchNextToMove fen)
      view oppoPieces fen `shouldBe` view likePieces (switchNextToMove fen)
