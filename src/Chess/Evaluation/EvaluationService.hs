module Chess.Evaluation.EvaluationService where

import Chess.Fen
import Chess.OpeningTable.OpeningTableAccessor

class (Monad m, OpeningTableAccessor m) =>
      EvaluationService m
  where
  calculateFenEvaluation :: FenRepresentation -> m Double
  evaluateFen :: FenRepresentation -> m Double
  evaluateFen fen = do
    queriedEvaluation <- lookupFenInOpeningTable fen
    case queriedEvaluation of
      Just eval -> return eval
      Nothing -> calculateFenEvaluation fen
