{-# LANGUAGE ParallelListComp #-}

module Moves.MoveLogicSpec where

import Control.Lens
import qualified Data.Set as Set
import Models.File
import Models.Move
import Models.Rank
import Models.Square
import Moves.MoveLogic
import Piece.Bishop
import Test.Hspec
import Models.Position
import Models.PieceOnSquare
import Models.Piece
import Models.PieceColour
import Models.PieceType
import Piece.Rook
import Piece.King
import Piece.Knight
import Piece.Pawn

spec = do
  describe "filterAllMoves" $ do
    it "correctly filter bishop sliding moves up to a like piece" $ do
      let
        start = Square Fa R1
        moves = emptyBoardMoves B start
        expected = [(Mv . Move start) (Square f r) |
           f <- [Fb .. Fg] |  r <- [R2 .. R7]]
        position = buildBasePosition [PieceOnSquare (Piece White King) (Square Fh R8)]
      filterAllMoves moves position `shouldMatchList` expected

    it "correctly filter bishop sliding moves up to a oppo piece" $ do
      let
        start = Square Fa R1
        moves = emptyBoardMoves B start
        expected = [(Mv . Move start) (Square f r) |
           f <- [Fb .. Fg] |  r <- [R2 .. R7]]
        position = buildBasePosition [PieceOnSquare (Piece Black King) (Square Fg R7)]
      filterAllMoves moves position `shouldMatchList` expected

    it "correctly filter rook sliding moves up to a like piece" $ do
      let
        start = Square Fa R1
        moves = emptyBoardMoves R start
        expected = [(Mv . Move start) (Square Fa r) | r <- [R2 .. R8]] ++ [(Mv . Move start) (Square f R1) | f <- [Fb .. Fg]]
        position = buildBasePosition [PieceOnSquare (Piece White King) (Square Fh R1)]
      filterAllMoves moves position `shouldMatchList` expected

    it "correctly filter rook sliding moves up to a oppo piece" $ do
      let
        start = Square Fa R1
        moves = emptyBoardMoves R start
        expected = [(Mv . Move start) (Square Fa r) | r <- [R2 .. R8]] ++ [(Mv . Move start) (Square f R1) | f <- [Fb .. Fg]]
        position = buildBasePosition [PieceOnSquare (Piece Black King) (Square Fg R1)]
      filterAllMoves moves position `shouldMatchList` expected

    it "correctly filter knight moves excluding like pieces" $ do
      let
        start = Square Fb R2
        likePieceSq = Square Fc R4
        moves = emptyBoardMoves N start
        expected = [(Mv . Move start) (Square f r) |
           (f, r) <- [(Fa, R4), (Fd, R3), (Fd, R1)]]
        position = buildBasePosition [PieceOnSquare (Piece White King) likePieceSq]
      filterAllMoves moves position `shouldMatchList` expected

    it "correctly filter knight moves including oppo pieces" $ do
      let
        start = Square Fb R2
        likePieceSq = Square Fc R4
        moves = emptyBoardMoves N start
        expected = [(Mv . Move start) (Square f r) |
           (f, r) <- [(Fa, R4), (Fc, R4), (Fd, R3), (Fd, R1)]]
        position = buildBasePosition [PieceOnSquare (Piece Black King) likePieceSq]
      filterAllMoves moves position `shouldMatchList` expected

    -- TODO check the other pawn moving filtering
    it "correctly filter pawn moves excluding like pieces" $ do
      let
        start = Square Fb R2
        likePieceSq = Square Fb R4
        moves = emptyBoardMoves P start
        expected = [(Mv . Move start) (Square Fb R3)]
        position = buildBasePosition [PieceOnSquare (Piece White King) likePieceSq]
      filterAllMoves moves position `shouldMatchList` expected

    it "correctly filter pawn moves including oppo pieces" $ do
      let
        start = Square Fb R2
        likePieceSq = Square Fb R4
        moves = emptyBoardMoves P start
        expected = [(Mv . Move start) (Square Fb R3)]
        position = buildBasePosition [PieceOnSquare (Piece Black King) likePieceSq]
      filterAllMoves moves position `shouldMatchList` expected

    -- TODO fix king move filtering when castling is included
    it "correctly filter king moves excluding like pieces" $ do
      let
        start = Square Fb R2
        likePieceSq = Square Fc R3
        moves = emptyBoardMoves K start
        expected = filter (\(Mv mv) -> view end mv `Set.notMember` Set.fromList [start, likePieceSq]) $ map (Mv . Move start) $ [Square f r | f <- [Fa .. Fc], r <- [R1 .. R3]]
        position = buildBasePosition [PieceOnSquare (Piece White King) likePieceSq]
      filterAllMoves moves position `shouldMatchList` expected

    it "correctly filter king moves including oppo pieces" $ do
      let
        start = Square Fb R2
        oppoPieceSq = Square Fc R3
        moves = emptyBoardMoves K start
        expected = filter (\(Mv mv) -> view end mv /= start) $ map (Mv . Move start) $ [Square f r | f <- [Fa .. Fc], r <- [R1 .. R3]]
        position = buildBasePosition [PieceOnSquare (Piece Black King) oppoPieceSq]
      filterAllMoves moves position `shouldMatchList` expected
