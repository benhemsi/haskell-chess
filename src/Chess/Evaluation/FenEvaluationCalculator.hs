module Chess.Evaluation.FenEvaluationCalculator where

import Chess.Fen
import Chess.OpeningTable.OpeningTableAccessor

class FenEvaluationCalculator m where
  calculateFenEvaluation :: FenRepresentation -> m Double
