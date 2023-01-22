module Models.Fen.FenParserSpec where

import Control.Lens
import Data.List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Validation
import Models.Board
import Models.Fen.CastlingPrivileges
import Models.Fen.FenParser
import Models.Fen.FenRepresentation
import Models.Piece
import Test.Hspec
import Test.Hspec.QuickCheck

spec = do
  describe "parsePieces" $ do
    prop "correctly parse an arbitrary list" $ \pl ->
      let deduplicatedPieces :: PieceList
          deduplicatedPieces = Map.elems $ Map.fromList $ map (\p -> (view square p, p)) pl
          Success actual = (parsePieces . show) deduplicatedPieces
       in actual `shouldMatchList` deduplicatedPieces
    it "correctly parse the starting piece list" $ do
      let expected = view pieces startingFenRepresentation
          Success actual = parsePieces "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
      actual `shouldMatchList` expected
    it "return an error if there are the wrong number of rows" $ do
      let expected = Failure (IncorrectRowNumber 2)
          actual = parsePieces "rnbqkbnr/pppppppp"
      actual `shouldBe` expected
    it "return an error if there are the wrong number of squares per row" $ do
      let expected = Failure (IncorrectSquaresPerRow 9)
          actual = parsePieces "rnbqkbnr1/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
      actual `shouldBe` expected
    it "return an error if there are invalid characters" $ do
      let expected = Failure (InvalidPieceListCharacters "f")
          actual = parsePieces "fnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
      actual `shouldBe` expected
  describe "parseNextToMove" $ do
    it "correctly parse w" $ do
      let actual = parseNextToMove "w"
      actual `shouldBe` Success White
    it "correctly parse b" $ do
      let actual = parseNextToMove "b"
      actual `shouldBe` Success Black
    it "return an error for all other strings" $ do
      let actual = parseNextToMove "wb"
      actual `shouldBe` Failure (InvalidNextToMove "wb")
  describe "parseCastlingPrivileges" $ do
    it "correctly parse KQkq" $ do
      let actual = parseCastlingPrivileges "KQkq"
      actual `shouldBe` Success (CastlingPrivileges True True True True)
    it "correctly parse -" $ do
      let actual = parseCastlingPrivileges "-"
      actual `shouldBe` Success (CastlingPrivileges False False False False)
    it "return an error for other strings" $ do
      let actual = parseCastlingPrivileges "qkQK"
      actual `shouldBe` Failure (InvalidCastlingPrivileges "qkQK")
  describe "parseEnPassentSquare" $ do
    it "correctly parse a3" $ do
      let actual = parseEnPassentSquare "a3"
      actual `shouldBe` Success (Just (Square Fa R3))
    it "correctly parse a6" $ do
      let actual = parseEnPassentSquare "a6"
      actual `shouldBe` Success (Just (Square Fa R6))
    it "correctly parse h3" $ do
      let actual = parseEnPassentSquare "h3"
      actual `shouldBe` Success (Just (Square Fh R3))
    it "correctly parse a6" $ do
      let actual = parseEnPassentSquare "h6"
      actual `shouldBe` Success (Just (Square Fh R6))
    it "correctly parse -" $ do
      let actual = parseEnPassentSquare "-"
      actual `shouldBe` Success Nothing
    it "return an error for other strings" $ do
      let actual = parseEnPassentSquare "ah6"
      actual `shouldBe` Failure (InvalidEnPassentSquare "ah6")
  describe "parseMoveClock" $ do
    it "correctly an int" $ do
      let actual = parseMoveClock "50"
      actual `shouldBe` Success 50
    it "return an error for all other strings" $ do
      let actual = parseMoveClock "hello"
      actual `shouldBe` Failure (InvalidMoveClock "hello")
    it "return an error for negative numbers" $ do
      let actual = parseMoveClock "-1"
      actual `shouldBe` Failure (InvalidMoveClock "-1")
  describe "parseFen" $ do
    it "correctly parse the starting FEN" $ do
      let Right (FenRepresentation pl n2mv cst enp half full) =
            parseFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
      pl `shouldMatchList` view pieces startingFenRepresentation
      n2mv `shouldBe` view nextToMove startingFenRepresentation
      cst `shouldBe` view castlingPrivileges startingFenRepresentation
      enp `shouldBe` view enPassentSquare startingFenRepresentation
      half `shouldBe` view halfMoveClock startingFenRepresentation
      full `shouldBe` view fullMoveClock startingFenRepresentation
    it "return an error when there are the wrong number of fields" $ do
      let actual = parseFen "a b"
      actual `shouldBe` Left (IncorrectFieldNumber 2)
    it "combine errors when there are a valid number of fields" $ do
      let actual = parseFen "a a a a a a"
      actual `shouldBe`
        Left
          (CombinedFenError $
           (IncorrectRowNumber 1) NE.:|
           [ InvalidNextToMove "a"
           , InvalidCastlingPrivileges "a"
           , InvalidEnPassentSquare "a"
           , InvalidMoveClock "a"
           , InvalidMoveClock "a"
           ])
