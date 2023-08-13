{-# LANGUAGE TemplateHaskell #-}

module Chess.Evaluation.EvaluationConfig where

import Chess.Evaluation.PieceWeightings
import Chess.OpeningTable.OpeningTableSettings (OpeningTableSettings)
import Control.Lens

data EvaluationConfig =
  EvaluationConfig
    { _openingTableSettings :: OpeningTableSettings
    , _pieceWeightings :: PieceWeightings
    }

makeLenses ''EvaluationConfig

updatePieceWeightingsInEvalConf :: PieceWeightings_ Maybe -> EvaluationConfig -> EvaluationConfig
updatePieceWeightingsInEvalConf newPW = over pieceWeightings (`pieceWeightingsWithDefault` newPW)
