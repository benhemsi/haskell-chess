{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chess.Evaluation.EvaluationReader where

import Chess.Evaluation.EvaluationConfig
import Chess.Evaluation.FenEvaluationCalculator
import Chess.Evaluation.PieceWeightings
import Chess.Fen
import Chess.OpeningTable.OpeningTableAccessor
import Chess.OpeningTable.OpeningTableReader
import Chess.Piece
import Control.Lens
import Control.Monad.Reader
import qualified Data.Map as Map

newtype EvaluationReader a =
  EvaluationReader
    { getEvaluationReader :: ReaderT EvaluationConfig IO a
    }
  deriving (Functor, Applicative, Monad)

liftOpeningTableReader :: OpeningTableReader a -> EvaluationReader a
liftOpeningTableReader openingTableAction = EvaluationReader output
  where
    output = do
      openingTableConf <- asks openingTableSettings
      let lookedUpValue = runReaderT (getOpeningTableReader openingTableAction) openingTableConf
      lift lookedUpValue

instance OpeningTableAccessor EvaluationReader where
  lookupFenInOpeningTable = liftOpeningTableReader . lookupFenInOpeningTable

instance FenEvaluationCalculator EvaluationReader where
  calculateFenEvaluation fen = EvaluationReader evaluation
    where
      evaluation = do
        weightings <- asks pieceWeightings
        return $ calculateFenPieceWeightings weightings fen
