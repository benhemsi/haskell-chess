module Chess.Evaluation.EvaluationService where

import Chess.Fen

class (Monad m) =>
      EvaluationService m
  where
  lookupFenInOpeningTable :: FenRepresentation -> m (Maybe Double)
  calculateFenEvaluation :: FenRepresentation -> m Double
  evaluateFen :: FenRepresentation -> m Double
  evaluateFen fen = do
    queriedEvaluation <- lookupFenInOpeningTable fen
    case queriedEvaluation of
      Just eval -> return eval
      Nothing -> calculateFenEvaluation fen
