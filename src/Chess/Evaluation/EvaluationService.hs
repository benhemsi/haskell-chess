module Chess.Evaluation.EvaluationService where

import Chess.Fen

class EvaluationService m where
  evaluateFen :: FenRepresentation -> m Double
