module Chess.Evaluation.EvaluationConfig where

import Chess.Evaluation.PieceWeightings
import Chess.OpeningTable.OpeningTableSettings (OpeningTableSettings)

data EvaluationConfig =
  EvaluationConfig
    { openingTableSettings :: OpeningTableSettings
    , pieceWeightings :: PieceWeightings
    }
