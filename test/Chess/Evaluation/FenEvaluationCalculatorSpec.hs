{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chess.Evaluation.FenEvaluationCalculatorSpec where

import Chess.Evaluation.FenEvaluationCalculator
import Chess.Fen (startingFenRepresentation)
import Chess.OpeningTable.OpeningTableAccessor
import Control.Monad.Reader
import Test.Hspec

data MockEvaluationExpectations =
  MockEvaluationExpectations
    { openingTableEvaluation :: Maybe Double
    , fenEvaluation :: Double
    }

newtype MockFenEvaluationCalculator a =
  MockFenEvaluationCalculator
    { unMock :: Reader MockEvaluationExpectations a
    }
  deriving (Functor, Applicative, Monad)

instance OpeningTableAccessor MockFenEvaluationCalculator where
  lookupFenInOpeningTable fen = MockFenEvaluationCalculator (asks openingTableEvaluation)

instance FenEvaluationCalculator MockFenEvaluationCalculator where
  calculateFenEvaluation fen = MockFenEvaluationCalculator (asks fenEvaluation)

runMock :: MockEvaluationExpectations -> Double
runMock = runReader (unMock readerToRun)
  where
    readerToRun = evaluateFen startingFenRepresentation

spec = do
  describe "evaluateFen" $ do
    it "query the opening table before evaluating the position" $ do
      let expectations = MockEvaluationExpectations (Just 1.0) 2.0
          actual = runMock expectations
          expected = 1.0
      actual `shouldBe` expected
    it "calculate the evaluation if the position does not exist in the opening table" $ do
      let expectations = MockEvaluationExpectations Nothing 2.0
          actual = runMock expectations
          expected = 2.0
      actual `shouldBe` expected
