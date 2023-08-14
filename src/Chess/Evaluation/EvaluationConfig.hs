{-# LANGUAGE TemplateHaskell #-}

module Chess.Evaluation.EvaluationConfig where

import Chess.Evaluation.PieceWeightings
import Chess.OpeningTable.OpeningTableSettings (OpeningTableSettings)
import Control.Concurrent.STM
import Control.Lens

data EvaluationConfig =
  EvaluationConfig
    { _openingTableSettings :: OpeningTableSettings
    , _pieceWeightings :: TVar PieceWeightings
    }

makeLenses ''EvaluationConfig

updatePieceWeightingsInEvalConf :: PieceWeightings_ Maybe -> EvaluationConfig -> STM EvaluationConfig
updatePieceWeightingsInEvalConf newPW currentConfig = do
  currentPieceWeightings <- readTVar (currentConfig ^. pieceWeightings)
  let newPieceWeightings = pieceWeightingsWithDefault currentPieceWeightings newPW
  _ <- writeTVar (currentConfig ^. pieceWeightings) newPieceWeightings
  return currentConfig
