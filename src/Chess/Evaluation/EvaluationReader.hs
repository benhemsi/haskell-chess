{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chess.Evaluation.EvaluationReader where

import Chess.Evaluation.EvaluationConfig
import Chess.Evaluation.EvaluationService
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

instance OpeningTableAccessor EvaluationReader where
  lookupFenInOpeningTable fen = EvaluationReader output
    where
      output = do
        openingTableConf <- asks openingTableSettings
        let lookedUpValue = runReaderT (getOpeningTableReader (lookupFenInOpeningTable fen)) openingTableConf
        lift lookedUpValue

instance EvaluationService EvaluationReader where
  calculateFenEvaluation fen = EvaluationReader evaluation
    where
      evaluation = do
        weightings <- asks pieceWeightings
        let whitePl = Map.elems (fen ^. pieces . whitePieces)
            blackPl = Map.elems (fen ^. pieces . blackPieces)
        return $ calculatePieceWeightings weightings whitePl - calculatePieceWeightings weightings blackPl
