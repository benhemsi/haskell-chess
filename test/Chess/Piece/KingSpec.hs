module Chess.Piece.KingSpec where

import Chess.Board
import Chess.Move
import Chess.Moves.MoveFiltering
import Chess.Piece.King
import qualified Data.Set as Set
import Test.Hspec

spec = do
  describe "emptyBoardMoves" $ do
    it "correctly calculate King moves" $ do
      let start = Square Fa R1
          moves = flattenMoves (emptyBoardMoves K start)
          expected = map (Mv . Move start) [Square Fa R2, Square Fb R1, Square Fb R2]
      moves `shouldMatchList` expected
    it "correctly calculate white King moves including castling" $ do
      let start = Square Fe R1
          moves = flattenMoves (emptyBoardMoves K start)
          standardMoves = map (Mv . Move start) [Square Fd R1, Square Fd R2, Square Fe R2, Square Ff R2, Square Ff R1]
          castlingMoves =
            map
              (\(kingMove, rookMove) -> Cst $ Castle kingMove rookMove)
              [ (Move start (Square Fg R1), Move (Square Fh R1) (Square Ff R1))
              , (Move start (Square Fc R1), Move (Square Fa R1) (Square Fd R1))
              ]
      moves `shouldMatchList` standardMoves ++ castlingMoves
    it "correctly calculate black King moves including castling" $ do
      let start = Square Fe R8
          moves = flattenMoves (emptyBoardMoves K start)
          standardMoves = map (Mv . Move start) [Square Fd R8, Square Fd R7, Square Fe R7, Square Ff R7, Square Ff R8]
          castlingMoves =
            map
              (\(kingMove, rookMove) -> Cst $ Castle kingMove rookMove)
              [ (Move start (Square Fg R8), Move (Square Fh R8) (Square Ff R8))
              , (Move start (Square Fc R8), Move (Square Fa R8) (Square Fd R8))
              ]
      moves `shouldMatchList` standardMoves ++ castlingMoves
