{-# LANGUAGE TemplateHaskell #-}

module Evaluation.Evaluation where

import Control.Lens
import Control.Monad.State
import Evaluation.EvaluationBoard
import Models.PieceColour

data Evaluation =
  Evaluation
    { _positionEvaluation :: Double
    , _evaluationColour :: PieceColour
    }

makeLenses ''Evaluation

instance Semigroup Evaluation where
  Evaluation x White <> Evaluation y White = Evaluation (max x y) White
  Evaluation x Black <> Evaluation y Black = Evaluation (min x y) Black

instance Monoid Evaluation where
  mempty = Evaluation 0 White
