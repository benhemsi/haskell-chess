{-# LANGUAGE ParallelListComp #-}

module Moves.MoveLogicSpec where

import Control.Lens
import qualified Data.Set as Set
import Models.FenRepresentation
import Models.File
import Models.Move
import Models.Piece
import Models.PieceColour
import Models.PieceOnSquare
import Models.PieceType
import Models.Position
import Models.Rank
import Models.Square
import Moves.MoveLogic
import Piece.Bishop
import Piece.King
import Piece.Knight
import Piece.Pawn
import Piece.Rook
import Test.Hspec

spec = do
  describe "filterSlidingMoves" $ do
    it "correctly filter bishop sliding moves up to a like piece" $ do
      let start = Square Fa R1
          moves = emptyBoardMoves B start
          expected = [(Mv . Move start) (Square f r) | f <- [Fb .. Fg] | r <- [R2 .. R7]]
          position = buildBasePosition [PieceOnSquare (Piece White King) (Square Fh R8)]
      filterAllMoves moves position `shouldMatchList` expected
    it "correctly filter bishop sliding moves up to a oppo piece" $ do
      let start = Square Fa R1
          moves = emptyBoardMoves B start
          expected = [(Mv . Move start) (Square f r) | f <- [Fb .. Fg] | r <- [R2 .. R7]]
          position = buildBasePosition [PieceOnSquare (Piece Black King) (Square Fg R7)]
      filterAllMoves moves position `shouldMatchList` expected
    it "correctly filter rook sliding moves up to a like piece" $ do
      let start = Square Fa R1
          moves = emptyBoardMoves R start
          expected =
            [(Mv . Move start) (Square Fa r) | r <- [R2 .. R8]] ++ [(Mv . Move start) (Square f R1) | f <- [Fb .. Fg]]
          position = buildBasePosition [PieceOnSquare (Piece White King) (Square Fh R1)]
      filterAllMoves moves position `shouldMatchList` expected
    it "correctly filter rook sliding moves up to a oppo piece" $ do
      let start = Square Fa R1
          moves = emptyBoardMoves R start
          expected =
            [(Mv . Move start) (Square Fa r) | r <- [R2 .. R8]] ++ [(Mv . Move start) (Square f R1) | f <- [Fb .. Fg]]
          position = buildBasePosition [PieceOnSquare (Piece Black King) (Square Fg R1)]
      filterAllMoves moves position `shouldMatchList` expected
  describe "filterMoves" $ do
    it "correctly filter knight moves excluding like pieces" $ do
      let start = Square Fb R2
          likePieceSq = Square Fc R4
          moves = emptyBoardMoves N start
          expected = [(Mv . Move start) (Square f r) | (f, r) <- [(Fa, R4), (Fd, R3), (Fd, R1)]]
          position = buildBasePosition [PieceOnSquare (Piece White King) likePieceSq]
      filterAllMoves moves position `shouldMatchList` expected
    it "correctly filter knight moves including oppo pieces" $ do
      let start = Square Fb R2
          likePieceSq = Square Fc R4
          moves = emptyBoardMoves N start
          expected = [(Mv . Move start) (Square f r) | (f, r) <- [(Fa, R4), (Fc, R4), (Fd, R3), (Fd, R1)]]
          position = buildBasePosition [PieceOnSquare (Piece Black King) likePieceSq]
      filterAllMoves moves position `shouldMatchList` expected
  describe "filterPawnMoves" $ do
    it "correctly filter out two-step pawn moves when a like piece occupies the square" $ do
      let start = Square Fb R2
          likePieceSq = Square Fb R4
          moves = emptyBoardMoves P start
          expected = [(Mv . Move start) (Square Fb R3)]
          position = buildBasePosition [PieceOnSquare (Piece White King) likePieceSq]
      filterAllMoves moves position `shouldMatchList` expected
    it "correctly filter out two-step pawn moves when an oppo piece occupies the square" $ do
      let start = Square Fb R2
          oppoPieceSq = Square Fb R4
          moves = emptyBoardMoves P start
          expected = [(Mv . Move start) (Square Fb R3)]
          position = buildBasePosition [PieceOnSquare (Piece Black King) oppoPieceSq]
      filterAllMoves moves position `shouldMatchList` expected
    it "correctly filter out one- and two-step pawn moves when a like piece occupies the next square" $ do
      let start = Square Fb R2
          likePieceSq = Square Fb R3
          moves = emptyBoardMoves P start
          position = buildBasePosition [PieceOnSquare (Piece White King) likePieceSq]
      filterAllMoves moves position `shouldSatisfy` null
    it "correctly filter out one- and two-step pawn moves when an oppo piece occupies the next square" $ do
      let start = Square Fb R2
          oppoPieceSq = Square Fb R3
          moves = emptyBoardMoves P start
          position = buildBasePosition [PieceOnSquare (Piece Black King) oppoPieceSq]
      filterAllMoves moves position `shouldSatisfy` null
    it "correctly include taking pawn moves when an oppo piece occupies a diagonal square" $ do
      let start = Square Fb R2
          oppoPieceSquares = [Square Fc R3, Square Fa R3, Square Fb R3]
          moves = emptyBoardMoves P start
          expected = [(Mv . Move start) (Square f r) | (f, r) <- [(Fc, R3), (Fa, R3)]]
          position = buildBasePosition $ map (PieceOnSquare (Piece Black King)) oppoPieceSquares
      filterAllMoves moves position `shouldMatchList` expected
    it "correctly filter out taking pawn moves when a like piece occupies a diagonal square" $ do
      let start = Square Fb R2
          oppoPieceSquares = [Square Fc R3, Square Fa R3, Square Fb R3]
          moves = emptyBoardMoves P start
          position = buildBasePosition $ map (PieceOnSquare (Piece White King)) oppoPieceSquares
      filterAllMoves moves position `shouldSatisfy` null
    it "correctly include en passent when the FEN representation has the en passent square" $ do
      let start = Square Fb R5
          oppoPieceSquares = [Square Fb R6, Square Fc R5]
          enPassentSq = Square Fc R6
          moves = emptyBoardMoves P start
          expected = [EnP $ EnPassent (Move start enPassentSq) (Square Fc R5)]
          position = buildBasePosition $ map (PieceOnSquare (Piece Black Pawn)) oppoPieceSquares
          positionWithEnPassent = set (fen . enPassentSquare) (Just enPassentSq) position
      filterAllMoves moves positionWithEnPassent `shouldMatchList` expected
    it "correctly exclude en passent when FEN representation does not have the en passent square" $ do
      let start = Square Fb R5
          oppoPieceSquares = [Square Fb R6, Square Fc R5]
          enPassentSq = Square Fc R6
          moves = emptyBoardMoves P start
          position = buildBasePosition $ map (PieceOnSquare (Piece Black Pawn)) oppoPieceSquares
      filterAllMoves moves position `shouldSatisfy` null
    it "correctly exclude en passent when FEN representation has the en passent square but the pawn cannot move there" $ do
      let start = Square Fb R4
          oppoPieceSq = Square Fb R5
          enPassentSq = Square Fc R6
          moves = emptyBoardMoves P start
          position = buildBasePosition [PieceOnSquare (Piece Black Pawn) oppoPieceSq]
          positionWithEnPassent = set (fen . enPassentSquare) (Just enPassentSq) position
      filterAllMoves moves positionWithEnPassent `shouldSatisfy` null
    it "correctly include promotion moves" $ do
      let start = Square Fb R7
          moves = emptyBoardMoves P start
          expected = [PP $ PawnPromotion (Move start (Square Fb R8))]
          position = buildBasePosition []
      filterAllMoves moves position `shouldMatchList` expected
    it "correctly include promotion taking moves" $ do
      let start = Square Fb R7
          oppoPieceSquares = [Square Fb R8, Square Fc R8]
          moves = emptyBoardMoves P start
          expected = [PP $ PawnPromotion (Move start (Square Fc R8))]
          position = buildBasePosition $ map (PieceOnSquare (Piece Black Knight)) oppoPieceSquares
      filterAllMoves moves position `shouldMatchList` expected
  describe "filterKingMoves" $
    -- TODO fix king move filtering when castling is included
   do
    it "correctly filter king moves excluding like pieces" $ do
      let start = Square Fb R2
          likePieceSq = Square Fc R3
          moves = emptyBoardMoves K start
          expected =
            filter (\(Mv mv) -> view end mv `Set.notMember` Set.fromList [start, likePieceSq]) $
            map (Mv . Move start) $ [Square f r | f <- [Fa .. Fc], r <- [R1 .. R3]]
          position = buildBasePosition [PieceOnSquare (Piece White King) likePieceSq]
      filterAllMoves moves position `shouldMatchList` expected
    it "correctly filter king moves including oppo pieces" $ do
      let start = Square Fb R2
          oppoPieceSq = Square Fc R3
          moves = emptyBoardMoves K start
          expected =
            filter (\(Mv mv) -> view end mv /= start) $
            map (Mv . Move start) $ [Square f r | f <- [Fa .. Fc], r <- [R1 .. R3]]
          position = buildBasePosition [PieceOnSquare (Piece Black King) oppoPieceSq]
      filterAllMoves moves position `shouldMatchList` expected
