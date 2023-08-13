{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chess.Evaluation.EvaluationService where

import Chess.Evaluation.EvaluationApi
import Chess.Evaluation.EvaluationConfig
import Chess.Evaluation.EvaluationRestApi
import Chess.Evaluation.FenEvaluationCalculator
import Chess.Evaluation.PieceWeightings
import Chess.Fen
import Chess.OpeningTable.OpeningTableAccessor
import Chess.OpeningTable.OpeningTableBuilder
import Chess.OpeningTable.OpeningTableService
import Chess.Piece
import Control.Lens
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Yaml as Y
import Servant.API
import Servant.Server

newtype EvaluationService a =
  EvaluationService
    { getEvaluationService :: StateT EvaluationConfig (LoggingT IO) a
    }
  deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadLoggerIO, MonadState EvaluationConfig)

instance OpeningTableAccessor EvaluationService where
  lookupFenInOpeningTable = liftOpeningTableReader . lookupFenInOpeningTable

instance FenEvaluationCalculator EvaluationService where
  calculateFenEvaluation fen = evaluation
    where
      evaluation = do
        weightings <- gets _pieceWeightings
        return $ calculateFenPieceWeightings weightings fen

instance EvaluationApi EvaluationService where
  evaluateFen = evaluateFenWithOpeningTable
  updatePieceWeightings newPW = modify (updatePieceWeightingsInEvalConf newPW)

liftOpeningTableReader :: OpeningTableService a -> EvaluationService a
liftOpeningTableReader openingTableAction = EvaluationService output
  where
    output = do
      openingTableConf <- gets _openingTableSettings
      let lookedUpValue = runReaderT (getOpeningTableService openingTableAction) openingTableConf
      lift lookedUpValue

evaluateFenWithOpeningTable ::
     (Monad m, OpeningTableAccessor m, FenEvaluationCalculator m) => FenRepresentation -> m Double
evaluateFenWithOpeningTable fen = do
  queriedEvaluation <- lookupFenInOpeningTable fen
  case queriedEvaluation of
    Just eval -> return eval
    Nothing -> calculateFenEvaluation fen

convertToHandler :: EvaluationConfig -> EvaluationService a -> Handler a
convertToHandler evalConfig evalReader = liftIO $ runStderrLoggingT ioOutput
  where
    ioOutput = evalStateT (getEvaluationService evalReader) evalConfig

evaluationReaderServer :: ServerT EvaluationRestApi EvaluationService
evaluationReaderServer = evaluateFen :<|> updatePieceWeightings

evalServer :: EvaluationConfig -> Server EvaluationRestApi
evalServer evalConfig = hoistServer evalApiProxy (convertToHandler evalConfig) evaluationReaderServer

createEvalApp :: FilePath -> Maybe FilePath -> IO Application
createEvalApp openingTableSettingsPath evalConfigPath = do
  openingTableSettings <- buildOpeningTable openingTableSettingsPath
  pieceWeightings <- maybe (pure defaultPieceWeightings) Y.decodeFileThrow evalConfigPath
  let evalConfig = EvaluationConfig openingTableSettings pieceWeightings
  return $ serve evalApiProxy (evalServer evalConfig)
