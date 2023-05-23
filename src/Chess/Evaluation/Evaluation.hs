{-# LANGUAGE TemplateHaskell #-}

module Chess.Evaluation.Evaluation where

import Chess.Piece
import Control.Lens

data Evaluation =
  Evaluation
    { _positionEvaluation :: Double
    , _evaluationColour :: PieceColour
    }

makeLenses ''Evaluation

instance Semigroup Evaluation where
  Evaluation x White <> Evaluation y White = Evaluation (max x y) White
  Evaluation x Black <> Evaluation y Black = Evaluation (min x y) Black
  Evaluation _ White <> Evaluation _ Black = Evaluation 0 White
  Evaluation _ Black <> Evaluation _ White = Evaluation 0 Black

instance Monoid Evaluation where
  mempty = Evaluation 0 White
