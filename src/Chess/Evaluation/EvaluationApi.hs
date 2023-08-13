module Chess.Evaluation.EvaluationApi where

import Chess.Evaluation.PieceWeightings (PieceWeightings_)
import Chess.Fen (FenRepresentation)

class EvaluationApi m where
  evaluateFen :: FenRepresentation -> m Double
  updatePieceWeightings :: PieceWeightings_ Maybe -> m ()
