{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chess.Evaluation.EvaluationServiceSpec where

import Chess.Evaluation.EvaluationService
import Chess.Fen (startingFenRepresentation)
import Control.Monad.Reader
import Test.Hspec

data MockEvaluationExpectations =
  MockEvaluationExpectations
    { openingTableEvaluation :: Maybe Double
    , fenEvaluation :: Double
    }

newtype MockEvaluationService a =
  MockEvaluationService
    { unMock :: Reader MockEvaluationExpectations a
    }
  deriving (Functor, Applicative, Monad)

instance EvaluationService MockEvaluationService where
  lookupFenInOpeningTable fen = MockEvaluationService (asks openingTableEvaluation)
  calculateFenEvaluation fen = MockEvaluationService (asks fenEvaluation)

runMock :: MockEvaluationExpectations -> Double
runMock = runReader (unMock readerToRun)
  where
    readerToRun = evaluateFen startingFenRepresentation

spec = do
  describe "EvaluationService" $ do
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
