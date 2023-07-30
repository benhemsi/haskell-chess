{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Chess.Evaluation.EvaluationApi where

import Chess.Evaluation.EvaluationReader
import Chess.Evaluation.FenEvaluationCalculator (FenEvaluationCalculator, calculateFenEvaluation)
import Chess.Fen (FenRepresentation)
import Chess.OpeningTable.OpeningTableAccessor

class EvaluationApi m where
  evaluateFen :: FenRepresentation -> m Double

evaluateFenWithOpeningTable ::
     (Monad m, OpeningTableAccessor m, FenEvaluationCalculator m) => FenRepresentation -> m Double
evaluateFenWithOpeningTable fen = do
  queriedEvaluation <- lookupFenInOpeningTable fen
  case queriedEvaluation of
    Just eval -> return eval
    Nothing -> calculateFenEvaluation fen

instance EvaluationApi EvaluationReader where
  evaluateFen = evaluateFenWithOpeningTable
