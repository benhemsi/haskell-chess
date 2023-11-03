module Chess.Evaluation.EvaluationApi where

import Chess.Evaluation.MinAndMaxEval
import Chess.Evaluation.PieceWeightings
import Chess.Fen (FenRepresentation)

class EvaluationApi m where
  evaluateFen :: FenRepresentation -> m Double
  updatePieceWeightings :: PieceWeightings_ Maybe -> m PieceWeightings
  evaluateFens :: [FenRepresentation] -> m (Maybe MinAndMaxEval)
