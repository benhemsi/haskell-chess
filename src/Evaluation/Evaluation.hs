{-# LANGUAGE TemplateHaskell #-}

module Evaluation.Evaluation where

import Control.Lens
import Control.Monad.State
import Evaluation.EvaluationBoard
import Models.PieceColour

data Evaluation =
  Evaluation
    { _positionEvaluation :: Double
    , _evaluationBoard :: EvaluationBoard
    }

makeLenses ''Evaluation
