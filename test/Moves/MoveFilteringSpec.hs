{-# LANGUAGE ParallelListComp #-}

module Moves.MoveFilteringSpec where

import Control.Lens
import qualified Data.Set as Set
import Models.Board
import Models.FenRepresentation
import Models.Move
import Models.Piece
import Models.Position
import Moves.MoveFiltering
import Piece.Bishop
import Piece.King
import Piece.Knight
import Piece.Pawn
import Piece.Rook
import Test.Hspec

spec = do
  describe "SlidingMoves" $ do
    it "correctly filter bishop sliding moves up to a like piece" $ do
      let start = Square Fa R1
          moves = emptyBoardMoves B start
          expected = [(Mv . Move start) (Square f r) | f <- [Fb .. Fg] | r <- [R2 .. R7]]
          position = buildBasePosition [PieceOnSquare (Piece White King) (Square Fh R8)]
      filterMoves moves position `shouldMatchList` expected
    it "correctly filter bishop sliding moves up to a oppo piece" $ do
      let start = Square Fa R1
          moves = emptyBoardMoves B start
          expected = [(Mv . Move start) (Square f r) | f <- [Fb .. Fg] | r <- [R2 .. R7]]
          position = buildBasePosition [PieceOnSquare (Piece Black King) (Square Fg R7)]
      filterMoves moves position `shouldMatchList` expected
    it "correctly filter rook sliding moves up to a like piece" $ do
      let start = Square Fa R1
          moves = emptyBoardMoves R start
          expected =
            [(Mv . Move start) (Square Fa r) | r <- [R2 .. R8]] ++ [(Mv . Move start) (Square f R1) | f <- [Fb .. Fg]]
          position = buildBasePosition [PieceOnSquare (Piece White King) (Square Fh R1)]
      filterMoves moves position `shouldMatchList` expected
    it "correctly filter rook sliding moves up to a oppo piece" $ do
      let start = Square Fa R1
          moves = emptyBoardMoves R start
          expected =
            [(Mv . Move start) (Square Fa r) | r <- [R2 .. R8]] ++ [(Mv . Move start) (Square f R1) | f <- [Fb .. Fg]]
          position = buildBasePosition [PieceOnSquare (Piece Black King) (Square Fg R1)]
      filterMoves moves position `shouldMatchList` expected
  describe "[Moves]" $ do
    it "correctly filter knight moves excluding like pieces" $ do
      let start = Square Fb R2
          likePieceSq = Square Fc R4
          moves = emptyBoardMoves N start
          expected = [(Mv . Move start) (Square f r) | (f, r) <- [(Fa, R4), (Fd, R3), (Fd, R1)]]
          position = buildBasePosition [PieceOnSquare (Piece White King) likePieceSq]
      filterMoves moves position `shouldMatchList` expected
    it "correctly filter knight moves including oppo pieces" $ do
      let start = Square Fb R2
          likePieceSq = Square Fc R4
          moves = emptyBoardMoves N start
          expected = [(Mv . Move start) (Square f r) | (f, r) <- [(Fa, R4), (Fc, R4), (Fd, R3), (Fd, R1)]]
          position = buildBasePosition [PieceOnSquare (Piece Black King) likePieceSq]
      filterMoves moves position `shouldMatchList` expected
  describe "getOppoAttackedSquares" $ do
    it "correctly get sliding attacked squares when there are no blocking pieces" $ do
      let start = Square Fa R1
          position = buildBasePosition [PieceOnSquare (Piece Black Rook) start]
          expected = Set.fromList $ [Square f R1 | f <- [Fb ..]] ++ [Square Fa r | r <- [R2 ..]]
      getOppoAttackedSquares position `shouldBe` expected
    it "correctly get sliding attacked squares when there are blocking pieces" $ do
      let start = Square Fa R1
          position =
            buildBasePosition
              [ PieceOnSquare (Piece Black Rook) start
              , PieceOnSquare (Piece Black Pawn) (Square Fa R2)
              , PieceOnSquare (Piece White Knight) (Square Fb R1)
              ]
          expected = Set.singleton (Square Fb R1)
      getOppoAttackedSquares position `shouldBe` expected
    it "correctly get pawn attacked squares" $ do
      let start = Square Fb R2
          position = buildBasePosition [PieceOnSquare (Piece Black Pawn) start]
          expected = Set.fromList [Square Fa R1, Square Fc R1]
      getOppoAttackedSquares position `shouldBe` expected
    it "correctly get king attacked squares" $ do
      let start = Square Fa R1
          position = buildBasePosition [PieceOnSquare (Piece Black King) start]
          expected = Set.fromList [Square Fa R2, Square Fb R2, Square Fb R1]
      getOppoAttackedSquares position `shouldBe` expected
    it "correctly get knight attacked squares" $ do
      let start = Square Fa R1
          position = buildBasePosition [PieceOnSquare (Piece Black Knight) start]
          expected = Set.fromList [Square Fb R3, Square Fc R2]
      getOppoAttackedSquares position `shouldBe` expected
    it "correctly get knight attacked squares when the squares are occupied" $ do
      let start = Square Fa R1
          position =
            buildBasePosition
              [PieceOnSquare (Piece Black Knight) start, PieceOnSquare (Piece White Pawn) (Square Fb R3)]
          expected = Set.fromList [Square Fb R3, Square Fc R2]
      getOppoAttackedSquares position `shouldBe` expected
  describe "PawnMoves" $ do
    it "correctly filter out two-step pawn moves when a like piece occupies the square" $ do
      let start = Square Fb R2
          likePieceSq = Square Fb R4
          moves = emptyBoardMoves P start
          expected = [(Mv . Move start) (Square Fb R3)]
          position = buildBasePosition [PieceOnSquare (Piece White King) likePieceSq]
      filterMoves moves position `shouldMatchList` expected
    it "correctly filter out two-step pawn moves when an oppo piece occupies the square" $ do
      let start = Square Fb R2
          oppoPieceSq = Square Fb R4
          moves = emptyBoardMoves P start
          expected = [(Mv . Move start) (Square Fb R3)]
          position = buildBasePosition [PieceOnSquare (Piece Black King) oppoPieceSq]
      filterMoves moves position `shouldMatchList` expected
    it "correctly filter out one- and two-step pawn moves when a like piece occupies the next square" $ do
      let start = Square Fb R2
          likePieceSq = Square Fb R3
          moves = emptyBoardMoves P start
          position = buildBasePosition [PieceOnSquare (Piece White King) likePieceSq]
      filterMoves moves position `shouldSatisfy` null
    it "correctly filter out one- and two-step pawn moves when an oppo piece occupies the next square" $ do
      let start = Square Fb R2
          oppoPieceSq = Square Fb R3
          moves = emptyBoardMoves P start
          position = buildBasePosition [PieceOnSquare (Piece Black King) oppoPieceSq]
      filterMoves moves position `shouldSatisfy` null
    it "correctly include taking pawn moves when an oppo piece occupies a diagonal square" $ do
      let start = Square Fb R2
          oppoPieceSquares = [Square Fc R3, Square Fa R3, Square Fb R3]
          moves = emptyBoardMoves P start
          expected = [(Mv . Move start) (Square f r) | (f, r) <- [(Fc, R3), (Fa, R3)]]
          position = buildBasePosition $ map (PieceOnSquare (Piece Black King)) oppoPieceSquares
      filterMoves moves position `shouldMatchList` expected
    it "correctly filter out taking pawn moves when a like piece occupies a diagonal square" $ do
      let start = Square Fb R2
          oppoPieceSquares = [Square Fc R3, Square Fa R3, Square Fb R3]
          moves = emptyBoardMoves P start
          position = buildBasePosition $ map (PieceOnSquare (Piece White King)) oppoPieceSquares
      filterMoves moves position `shouldSatisfy` null
    it "correctly include en passent when the FEN representation has the en passent square" $ do
      let start = Square Fb R5
          oppoPieceSquares = [Square Fb R6, Square Fc R5]
          enPassentSq = Square Fc R6
          moves = emptyBoardMoves P start
          expected = [EnP $ EnPassent (Move start enPassentSq) (Square Fc R5)]
          position = buildBasePosition $ map (PieceOnSquare (Piece Black Pawn)) oppoPieceSquares
          positionWithEnPassent = set (fen . enPassentSquare) (Just enPassentSq) position
      filterMoves moves positionWithEnPassent `shouldMatchList` expected
    it "correctly exclude en passent when FEN representation does not have the en passent square" $ do
      let start = Square Fb R5
          oppoPieceSquares = [Square Fb R6, Square Fc R5]
          enPassentSq = Square Fc R6
          moves = emptyBoardMoves P start
          position = buildBasePosition $ map (PieceOnSquare (Piece Black Pawn)) oppoPieceSquares
      filterMoves moves position `shouldSatisfy` null
    it "correctly exclude en passent when FEN representation has the en passent square but the pawn cannot move there" $ do
      let start = Square Fb R4
          oppoPieceSq = Square Fb R5
          enPassentSq = Square Fc R6
          moves = emptyBoardMoves P start
          position = buildBasePosition [PieceOnSquare (Piece Black Pawn) oppoPieceSq]
          positionWithEnPassent = set (fen . enPassentSquare) (Just enPassentSq) position
      filterMoves moves positionWithEnPassent `shouldSatisfy` null
    it "correctly include promotion moves" $ do
      let start = Square Fb R7
          moves = emptyBoardMoves P start
          expected = [PP $ PawnPromotion (Move start (Square Fb R8))]
          position = buildBasePosition []
      filterMoves moves position `shouldMatchList` expected
    it "correctly include promotion taking moves" $ do
      let start = Square Fb R7
          oppoPieceSquares = [Square Fb R8, Square Fc R8]
          moves = emptyBoardMoves P start
          expected = [PP $ PawnPromotion (Move start (Square Fc R8))]
          position = buildBasePosition $ map (PieceOnSquare (Piece Black Knight)) oppoPieceSquares
      filterMoves moves position `shouldMatchList` expected
  describe "KingMoves" $ do
    it "correctly filter king moves excluding like pieces" $ do
      let start = Square Fb R2
          likePieceSq = Square Fc R3
          moves = emptyBoardMoves K start
          expected =
            filter (\(Mv mv) -> view end mv `Set.notMember` Set.fromList [start, likePieceSq]) $
            map (Mv . Move start) $ [Square f r | f <- [Fa .. Fc], r <- [R1 .. R3]]
          position = buildBasePosition [PieceOnSquare (Piece White King) likePieceSq]
      filterMoves moves position `shouldMatchList` expected
    it "correctly filter king moves including oppo pieces" $ do
      let start = Square Fb R2
          oppoPieceSq = Square Fc R3
          moves = emptyBoardMoves K start
          expected =
            filter (\(Mv mv) -> view end mv /= start) $
            map (Mv . Move start) $ [Square f r | f <- [Fa .. Fc], r <- [R1 .. R3]]
          position = buildBasePosition [PieceOnSquare (Piece Black King) oppoPieceSq]
      filterMoves moves position `shouldMatchList` expected
    it "correctly filter king moves including castling moves when there are no pieces blocking" $ do
      let start = Square Fe R1
          moves = emptyBoardMoves K start
          standardMoves = map (Mv . Move start) [Square Fd R1, Square Fd R2, Square Fe R2, Square Ff R2, Square Ff R1]
          castlingMoves =
            map
              (\(kingMove, rookMove) -> Cst $ Castle kingMove rookMove)
              [ (Move start (Square Fg R1), Move (Square Fh R1) (Square Ff R1))
              , (Move start (Square Fc R1), Move (Square Fa R1) (Square Fd R1))
              ]
          position = buildBasePosition []
      filterMoves moves position `shouldMatchList` standardMoves ++ castlingMoves
    it "correctly filter king moves excluding castling moves when there are pieces occupying the castling squares" $ do
      let start = Square Fe R1
          moves = emptyBoardMoves K start
          standardMoves = map (Mv . Move start) [Square Fd R1, Square Fd R2, Square Fe R2, Square Ff R2, Square Ff R1]
          position =
            buildBasePosition
              [PieceOnSquare (Piece Black Bishop) (Square Ff R1), PieceOnSquare (Piece White Bishop) (Square Fc R1)]
      filterMoves moves position `shouldMatchList` standardMoves
    it
      "correctly filter king moves excluding castling moves when there are sliding pieces attacking the castling squares" $ do
      let start = Square Fe R1
          moves = emptyBoardMoves K start
          standardMoves = map (Mv . Move start) [Square Fd R1, Square Fd R2, Square Fe R2, Square Ff R2, Square Ff R1]
          position =
            buildBasePosition
              [PieceOnSquare (Piece Black Bishop) (Square Fa R3), PieceOnSquare (Piece Black Rook) (Square Fc R1)]
      filterMoves moves position `shouldMatchList` standardMoves
    it
      "correctly filter king moves including castling moves when there are blocked sliding pieces attacking the castling squares" $ do
      let start = Square Fe R1
          moves = emptyBoardMoves K start
          standardMoves = map (Mv . Move start) [Square Fd R1, Square Fd R2, Square Fe R2, Square Ff R2, Square Ff R1]
          castlingMoves =
            map
              (\(kingMove, rookMove) -> Cst $ Castle kingMove rookMove)
              [ (Move start (Square Fg R1), Move (Square Fh R1) (Square Ff R1))
              , (Move start (Square Fc R1), Move (Square Fa R1) (Square Fd R1))
              ]
          position =
            buildBasePosition
              [ PieceOnSquare (Piece Black Bishop) (Square Fa R3)
              , PieceOnSquare (Piece Black Rook) (Square Ff R8)
              , PieceOnSquare (Piece White Pawn) (Square Fb R2)
              , PieceOnSquare (Piece White Pawn) (Square Ff R3)
              ]
      filterMoves moves position `shouldMatchList` standardMoves ++ castlingMoves
