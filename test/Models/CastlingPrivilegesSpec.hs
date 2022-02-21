module Models.CastlingPrivilegesSpec where

import qualified Data.Set as Set
import Models.ArbitraryInstances
import Models.CastlingPrivileges
import Models.File
import Models.FullPieceList
import Models.Move
import Models.Piece
import Models.PieceColour (PieceColour (..))
import Models.PieceOnSquare
import Models.PieceType
import Models.Rank
import Models.Square
import Test.Hspec
import Test.Hspec.QuickCheck
import Text.Read

spec = do
  describe "read" $ do
    prop "is inverse to show" $ do
      pending
      -- \x -> (read . show) x `shouldBe` (x :: CastlingPrivileges)

--     it "throw an error when the castling privileges are in the wrong order" $ do
--       let result = readMaybe "kK" :: Maybe CastlingPrivileges
--       result `shouldBe` Nothing

--     it "throw an error for an incorrect string" $ do
--       let result = readMaybe "a" :: Maybe CastlingPrivileges
--       result `shouldBe` Nothing

--   describe "getCastlingMoves" $ do
--     it "correctly return white king side and queen side moves on an empty board" $ do
--       let start = Square Fe R1
--           fullPL = FullPieceList [PieceOnSquare (Piece White King) start] [] Set.empty Set.empty Set.empty Set.empty
--           moves = getCastlingMoves White fullPL (CastlingPrivileges True True False False)
--       moves `shouldMatchList` [getKingSideCastle White, getQueenSideCastle White]

--     it "correctly return black king side and queen side moves on an empty board" $ do
--       let start = Square Fe R8
--           fullPL = FullPieceList [] [PieceOnSquare (Piece Black King) start] Set.empty Set.empty Set.empty Set.empty
--           moves = getCastlingMoves Black fullPL (CastlingPrivileges False False True True)
--       moves `shouldMatchList` [getKingSideCastle Black, getQueenSideCastle Black]

--     it "return no moves when the castling privileges are false" $ do
--       let start = Square Fe R1
--           fullPL = FullPieceList [PieceOnSquare (Piece White King) start] [] Set.empty Set.empty Set.empty Set.empty
--           moves = getCastlingMoves White fullPL (CastlingPrivileges False False False False)
--       moves `shouldMatchList` []

--     it "return no moves when there is a like piece blocking castling" $ do
--       let start = Square Fe R1
--           fullPL = FullPieceList [PieceOnSquare (Piece White King) start] [] (Set.fromList [Square Fb R1, Square Fg R1]) Set.empty Set.empty Set.empty
--           moves = getCastlingMoves White fullPL (CastlingPrivileges True True False False)
--       moves `shouldMatchList` []

--     it "return no moves when there is an oppo piece blocking castling" $ do
--       let start = Square Fe R1
--           fullPL = FullPieceList [PieceOnSquare (Piece White King) start] [] Set.empty (Set.fromList [Square Fb R1, Square Fg R1]) Set.empty Set.empty
--           moves = getCastlingMoves White fullPL (CastlingPrivileges True True False False)
--       moves `shouldMatchList` []

--     it "return no moves when there is an oppo piece preventing castling" $ do
--       let start = Square Fe R1
--           fullPL = FullPieceList [PieceOnSquare (Piece White King) start] [] Set.empty Set.empty Set.empty (Set.fromList [Square Fc R1, Square Fg R1])
--           moves = getCastlingMoves White fullPL (CastlingPrivileges True True False False)
--       moves `shouldMatchList` []

--     it "return queen side castle move when the oppo piece attacks b1" $ do
--       let start = Square Fe R1
--           fullPL = FullPieceList [PieceOnSquare (Piece White King) start] [] Set.empty Set.empty Set.empty (Set.fromList [Square Fb R1, Square Fg R1])
--           moves = getCastlingMoves White fullPL (CastlingPrivileges True True False False)
--       moves `shouldMatchList` [getQueenSideCastle White]
