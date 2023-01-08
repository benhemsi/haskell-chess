module Models.PositionSpec where
  
import Control.Lens
import Models.Position
import Models.Piece
import Test.Hspec

pos = buildBasePosition startingPieceList

spec = do
  describe "likePieces" $ do
    it "fetch like pieces" $ do
      view likePieces pos `shouldBe` filter (\p -> view (piece.pieceColour) p == White) startingPieceList
    it "obey the view set law" $ do
      view likePieces (set likePieces [] pos) `shouldBe` []
    it "obey the set view law" $ do
      set likePieces (view likePieces pos) pos `shouldBe` pos
    it "obey the set set law" $ do
      set likePieces [] (set likePieces startingPieceList pos) `shouldBe` set likePieces [] pos
  describe "oppoPieces" $ do
    it "fetch oppo pieces" $ do
      view oppoPieces pos `shouldBe` filter (\p -> view (piece.pieceColour) p == Black) startingPieceList
  describe "switchNextToMove" $ do
    it "work with the lenses" $ do
      view likePieces pos `shouldBe` view oppoPieces (switchNextToMove pos)
      view oppoPieces pos `shouldBe` view likePieces (switchNextToMove pos)
