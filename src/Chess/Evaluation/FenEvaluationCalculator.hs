module Chess.Evaluation.FenEvaluationCalculator where

import Chess.Fen
import Chess.OpeningTable.OpeningTableAccessor

class FenEvaluationCalculator m where
  calculateFenEvaluation :: FenRepresentation -> m Double

evaluateFen :: (Monad m, OpeningTableAccessor m, FenEvaluationCalculator m) => FenRepresentation -> m Double
evaluateFen fen = do
  queriedEvaluation <- lookupFenInOpeningTable fen
  case queriedEvaluation of
    Just eval -> return eval
    Nothing -> calculateFenEvaluation fen
