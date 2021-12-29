module MovesSpec where
import Moves
import Square
import Test.Hspec

spec = do
  describe "kingMoves" $ do
    it "correctly calculate King moves" $ do
      let moves = kingMoves (emptyBoardMoves (square Fa R1))
      map (show . end) moves `shouldMatchList` ["a2", "b1", "b2"]

  describe "rookMoves" $ do
    it "correctly calculate the north and east moves" $ do
      let moves = flattenRookMoves (rookMoves (emptyBoardMoves (square Fa R1)))
      map (show . end) moves `shouldMatchList` map (("a" ++) . show) [2..8] ++ map (: "1") ['b'..'h']

    it "correctly calculate the south and west moves" $ do
      let moves = flattenRookMoves (rookMoves (emptyBoardMoves (square Fh R8)))
      map (show . end) moves `shouldMatchList` map (("h" ++) . show) [1..7] ++ map (: "8") ['a'..'g']

  describe "bishopMoves" $ do
    it "correctly calculate the northEast diagonal" $ do
      let moves = flattenBishopMoves (bishopMoves (emptyBoardMoves (square Fa R1)))
      map (show . end) moves `shouldMatchList` zipWith (\ f r -> f : show r) ['b'..'h'] [2..8]

    it "correctly calculate the southEast diagonal" $ do
      let moves = flattenBishopMoves (bishopMoves (emptyBoardMoves (square Fa R8)))
      map (show . end) moves `shouldMatchList` zipWith (\ f r -> f : show r) ['b'..'h'] (reverse [1..7])

    it "correctly calculate the southWest diagonal" $ do
      let moves = flattenBishopMoves (bishopMoves (emptyBoardMoves (square Fh R8)))
      map (show . end) moves `shouldMatchList` zipWith (\ f r -> f : show r) ['a'..'g'] [1..7]

    it "correctly calculate the northWest diagonal" $ do
      let moves = flattenBishopMoves (bishopMoves (emptyBoardMoves (square Fh R1)))
      map (show . end) moves `shouldMatchList` zipWith (\ f r -> f : show r) (reverse ['a'..'g']) [2..8]

  describe "queenMoves" $ do
    it "correctly combine rook and bishop moves" $ do
      let moves = flattenQueenMoves (queenMoves (emptyBoardMoves (square Fa R1)))
      map (show . end) moves `shouldMatchList` map (("a" ++) . show) [2..8] ++ map (: "1") ['b'..'h'] ++ zipWith (\ f r -> f : show r) ['b'..'h'] [2..8]

  describe "knightMoves" $ do
    it "correctly calculate the knight jumps" $ do
      let moves = knightMoves (emptyBoardMoves (square Fa R1))
      map (show . end) moves `shouldMatchList` ["c2", "b3"]

  describe "pawnMoves" $ do
    it "correctly calculate the white pawn moves" $ do
      let moves = pawnMoves (emptyBoardMoves (square Fb R2))
      map (show . end) (white moves) `shouldMatchList` ["a3", "b3", "c3"]

    it "correctly calculate the black pawn moves" $ do
      let moves = pawnMoves (emptyBoardMoves (square Fb R2))
      map (show . end) (black moves) `shouldMatchList` ["a1", "b1", "c1"]

    it "return no moves if on the first rank" $ do
      let moves = pawnMoves (emptyBoardMoves (square Fb R1))
      white moves ++ black moves `shouldMatchList` []

    it "return no moves if on the eigth rank" $ do
      let moves = pawnMoves (emptyBoardMoves (square Fb R8))
      white moves ++ black moves `shouldMatchList` []
