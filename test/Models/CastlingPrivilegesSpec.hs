module Models.CastlingPrivilegesSpec where

import qualified Data.Set as Set
import Models.CastlingPrivileges
import Models.File
import Models.FullPieceList
import Models.Move
import Models.Piece
import Models.PieceColour (PieceColour(..))
import Models.PieceOnSquare
import Models.PieceType
import Models.Rank
import Models.Square
import Test.Hspec
import Test.Hspec.QuickCheck
import Text.Read

spec = do
  describe "read" $ do prop "is inverse to show" $ \x -> (read . show) x `shouldBe` (x :: CastlingPrivileges)
