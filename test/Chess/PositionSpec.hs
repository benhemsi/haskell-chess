module Chess.PositionSpec where

import Control.Lens
import qualified Data.Map as Map
import Chess.Fen
import Chess.Piece
import Chess.Position
import Test.Hspec

pos = buildBasePosition startingPieceList

spec = do
  describe "likePieces" $ do
    it "fetch like pieces" $ do view likePieces pos `shouldBe` pos ^. fen . pieces . whitePieces
    it "obey the view set law" $ do view likePieces (set likePieces Map.empty pos) `shouldBe` Map.empty
    it "obey the set view law" $ do set likePieces (view likePieces pos) pos `shouldBe` pos
    it "obey the set set law" $ do
      set likePieces Map.empty (set likePieces (startingPieceList ^. whitePieces) pos) `shouldBe`
        set likePieces Map.empty pos
  describe "oppoPieces" $ do
    it "fetch oppo pieces" $ do view oppoPieces pos `shouldBe` pos ^. fen . pieces . blackPieces
  describe "switchNextToMove" $ do
    it "work with the lenses" $ do
      view likePieces pos `shouldBe` view oppoPieces (switchNextToMove pos)
      view oppoPieces pos `shouldBe` view likePieces (switchNextToMove pos)
