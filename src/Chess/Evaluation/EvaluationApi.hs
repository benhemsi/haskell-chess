{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Chess.Evaluation.EvaluationApi where

import Chess.Evaluation.FenEvaluationCalculator (FenEvaluationCalculator, calculateFenEvaluation)
import Chess.Fen (FenRepresentation)
import Chess.OpeningTable.OpeningTableAccessor

class EvaluationApi m where
  evaluateFen :: FenRepresentation -> m Double
