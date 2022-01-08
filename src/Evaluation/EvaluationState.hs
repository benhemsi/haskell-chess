module Evaluation.EvaluationState where

import Control.Monad.State
import Evaluation.EvaluationBoard
import Models.PieceColour

data Evaluation = Evaluation {nextToMove :: PieceColour, positionEvaluation :: Double, evaluationBoard :: EvaluationBoard}

type EvaluationState = State Evaluation
