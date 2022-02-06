module Piece.BishopSpec where

import qualified Data.Set as Set
import Models.File
import Models.Move
import Models.Rank
import Models.Square
import Piece.Bishop
import Test.Hspec

spec = do
  describe "emptyBoardMoves" $ do
    it "correctly calculate the northEast diagonal" $ do
      let moves = flattenMoves (emptyBoardMoves (Square Fa R1))
      map (show . end) moves `shouldMatchList` zipWith (\f r -> f : show r) ['b' .. 'h'] [2 .. 8]

    it "correctly calculate the southEast diagonal" $ do
      let moves = flattenMoves (emptyBoardMoves (Square Fa R8))
      map (show . end) moves `shouldMatchList` zipWith (\f r -> f : show r) ['b' .. 'h'] (reverse [1 .. 7])

    it "correctly calculate the southWest diagonal" $ do
      let moves = flattenMoves (emptyBoardMoves (Square Fh R8))
      map (show . end) moves `shouldMatchList` zipWith (\f r -> f : show r) ['a' .. 'g'] [1 .. 7]

    it "correctly calculate the northWest diagonal" $ do
      let moves = flattenMoves (emptyBoardMoves (Square Fh R1))
      map (show . end) moves `shouldMatchList` zipWith (\f r -> f : show r) (reverse ['a' .. 'g']) [2 .. 8]

  describe "validMoves" $ do
    it "correctly filter up to but not including a like piece on the same diagonal" $ do
      let moves = validMoves (emptyBoardMoves (Square Fa R1)) (Set.singleton (Square Fc R3)) Set.empty
      map (show . end) moves `shouldMatchList` ["b2"]

    it "correctly filter up to and including an oppo piece on the same diagonal" $ do
      let moves = validMoves (emptyBoardMoves (Square Fa R1)) Set.empty (Set.singleton (Square Fc R3))
      map (show . end) moves `shouldMatchList` ["b2", "c3"]

    it "correctly filter when there are multiple pieces on the same diagonal" $ do
      let moves = validMoves (emptyBoardMoves (Square Fa R1)) (Set.fromList [Square Fe R5, Square Ff R6]) (Set.fromList [Square Fc R3, Square Fd R4])
      map (show . end) moves `shouldMatchList` ["b2", "c3"]
