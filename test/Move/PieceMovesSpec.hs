module Move.PieceMovesSpec where

import Models.File
import Models.Move
import Models.Rank
import Models.Square
import Move.PieceMoves as P
import Move.PawnMoves
import Move.RookMoves as R
import Move.BishopMoves as B
import Move.QueenMoves as Q
import Test.Hspec

spec = do
  describe "kingMoves" $ do
    it "correctly calculate King moves" $ do
      let moves = kingMoves (P.emptyBoardMoves (Square Fa R1))
      map (show . end) moves `shouldMatchList` ["a2", "b1", "b2"]

  describe "rookMoves" $ do
    it "correctly calculate the north and east moves" $ do
      let moves = R.flattenMoves (rookMoves (P.emptyBoardMoves (Square Fa R1)))
      map (show . end) moves `shouldMatchList` map (("a" ++) . show) [2 .. 8] ++ map (: "1") ['b' .. 'h']

    it "correctly calculate the south and west moves" $ do
      let moves = R.flattenMoves (rookMoves (P.emptyBoardMoves (Square Fh R8)))
      map (show . end) moves `shouldMatchList` map (("h" ++) . show) [1 .. 7] ++ map (: "8") ['a' .. 'g']

  describe "bishopMoves" $ do
    it "correctly calculate the northEast diagonal" $ do
      let moves = B.flattenMoves (bishopMoves (P.emptyBoardMoves (Square Fa R1)))
      map (show . end) moves `shouldMatchList` zipWith (\f r -> f : show r) ['b' .. 'h'] [2 .. 8]

    it "correctly calculate the southEast diagonal" $ do
      let moves = B.flattenMoves (bishopMoves (P.emptyBoardMoves (Square Fa R8)))
      map (show . end) moves `shouldMatchList` zipWith (\f r -> f : show r) ['b' .. 'h'] (reverse [1 .. 7])

    it "correctly calculate the southWest diagonal" $ do
      let moves = B.flattenMoves (bishopMoves (P.emptyBoardMoves (Square Fh R8)))
      map (show . end) moves `shouldMatchList` zipWith (\f r -> f : show r) ['a' .. 'g'] [1 .. 7]

    it "correctly calculate the northWest diagonal" $ do
      let moves = B.flattenMoves (bishopMoves (P.emptyBoardMoves (Square Fh R1)))
      map (show . end) moves `shouldMatchList` zipWith (\f r -> f : show r) (reverse ['a' .. 'g']) [2 .. 8]

  describe "queenMoves" $ do
    it "correctly combine rook and bishop moves" $ do
      let moves = Q.flattenMoves (queenMoves (P.emptyBoardMoves (Square Fa R1)))
      map (show . end) moves `shouldMatchList` map (("a" ++) . show) [2 .. 8] ++ map (: "1") ['b' .. 'h'] ++ zipWith (\f r -> f : show r) ['b' .. 'h'] [2 .. 8]

  describe "knightMoves" $ do
    it "correctly calculate the knight jumps" $ do
      let moves = knightMoves (P.emptyBoardMoves (Square Fa R1))
      map (show . end) moves `shouldMatchList` ["c2", "b3"]

  describe "pawnMoves" $ do
    it "correctly calculate the white pawn moves" $ do
      let moves = pawnMoves (P.emptyBoardMoves (Square Fb R2))
      map (show . end) (white moves) `shouldMatchList` ["a3", "b3", "c3"]

    it "correctly calculate the black pawn moves" $ do
      let moves = pawnMoves (P.emptyBoardMoves (Square Fb R2))
      map (show . end) (black moves) `shouldMatchList` ["a1", "b1", "c1"]

    it "return no moves if on the first rank" $ do
      let moves = pawnMoves (P.emptyBoardMoves (Square Fb R1))
      white moves ++ black moves `shouldMatchList` []

    it "return no moves if on the eigth rank" $ do
      let moves = pawnMoves (P.emptyBoardMoves (Square Fb R8))
      white moves ++ black moves `shouldMatchList` []
