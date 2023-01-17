module Models.Fen.FenParserSpec where

import Control.Lens
import Data.List
import qualified Data.Map as Map
import Data.Validation
import Models.Fen.FenParser
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
