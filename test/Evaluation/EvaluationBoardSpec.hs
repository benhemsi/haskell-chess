module Evaluation.EvaluationBoardSpec where

import qualified Data.Set as Set
import Evaluation.EvaluationBoard
import Chess.Board
import Chess.Piece
import Chess.Position (Position)
import Test.Hspec

spec = do
  describe "buildEvaluationBoard" $ do it "correctly build an evaluation board from a full piece list" $ do pending
      -- let 
      --   whitePiece = PieceOnSquare (Piece White Rook)  (Square Fa R1)
      --   blackPiece = PieceOnSquare (Piece Black Rook) (Square Fh R8)
      --   evaluationBoard = buildEvaluationBoard (FullPieceList [whitePiece] [blackPiece] Set.empty Set.empty Set.empty Set.empty)
      -- getAttackers (Square Fa R8) White evaluationBoard `shouldMatchList` [whitePiece]
      -- getAttackers (Square Fa R8) Black evaluationBoard `shouldMatchList` [blackPiece]
      -- checkIfSquareAttacked (Square Fa R8) Black evaluationBoard `shouldBe` True
      -- getNumberOfAttackers (Square Fa R8) Black evaluationBoard `shouldBe` 1
