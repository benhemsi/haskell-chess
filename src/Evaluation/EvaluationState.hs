{-# LANGUAGE TemplateHaskell #-}

module Evaluation.EvaluationState where

import Control.Lens
import Control.Monad.State
import Evaluation.EvaluationBoard
import Models.PieceColour

data Evaluation = Evaluation {_nextToMove :: PieceColour, _positionEvaluation :: Double, _evaluationBoard :: EvaluationBoard}

makeLenses ''Evaluation

type EvaluationState = State Evaluation
