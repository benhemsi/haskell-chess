module Piece.QueenSpec where

import qualified Data.Set as Set
import Models.File
import Models.Move
import Models.Rank
import Models.Square
import Piece.Queen
import Test.Hspec

spec = do
  describe "emptyBoardMoves" $ do
    it "correctly combine rook and bishop moves" $ do
      let moves = flattenMoves (emptyBoardMoves (Square Fa R1))
      map (show . end) moves `shouldMatchList` map (("a" ++) . show) [2 .. 8] ++ map (: "1") ['b' .. 'h'] ++ zipWith (\f r -> f : show r) ['b' .. 'h'] [2 .. 8]

  describe "validMoves" $ do
    it "correctly combine rook and bishop moves" $ do
      let moves = validMoves (emptyBoardMoves (Square Fa R1)) (Set.fromList [Square Fc R3, Square Fa R3, Square Fc R1]) Set.empty
      map (show . end) moves `shouldMatchList` ["a2", "b1", "b2"]
