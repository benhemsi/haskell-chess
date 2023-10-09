{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chess.Evaluation.EvaluationService where

import Chess.Evaluation.EvaluationApi
import Chess.Evaluation.EvaluationConfig
import Chess.Evaluation.EvaluationRestApi
import Chess.Evaluation.FenEvaluationCalculator
import Chess.Evaluation.PieceWeightings
import Chess.Evaluation.ServantTypeclassInstances
import Chess.Fen
import Chess.OpeningTable.OpeningTableAccessor
import Chess.OpeningTable.OpeningTableBuilder
import Chess.OpeningTable.OpeningTableService
import Chess.Piece
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Data.Yaml as Y
import Servant.API
import Servant.Server
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import qualified Streamly.Prelude as Stream

newtype EvaluationService a =
  EvaluationService
    { getEvaluationService :: ReaderT EvaluationConfig (LoggingT IO) a
    }
  deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadLoggerIO, MonadReader EvaluationConfig)

instance OpeningTableAccessor EvaluationService where
  lookupFenInOpeningTable = liftOpeningTableReader . lookupFenInOpeningTable

instance FenEvaluationCalculator EvaluationService where
  calculateFenEvaluation fen = evaluation
    where
      evaluation = do
        weightings <- asks _pieceWeightings
        pws <- liftIO (readTVarIO weightings)
        return $ calculateFenPieceWeightings pws fen

instance EvaluationApi EvaluationService where
  evaluateFen = evaluateFenWithOpeningTable
  updatePieceWeightings newPW = do
    currentEvalConf <- ask
    newEvalConf <- liftIO $ atomically (updatePieceWeightingsInEvalConf newPW currentEvalConf)
    liftIO $ readTVarIO (newEvalConf ^. pieceWeightings)

liftOpeningTableReader :: OpeningTableService a -> EvaluationService a
liftOpeningTableReader openingTableAction = EvaluationService output
  where
    output = do
      openingTableConf <- asks _openingTableSettings
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
    ioOutput = runReaderT (getEvaluationService evalReader) evalConfig

evaluationReaderServer :: ServerT EvaluationRestApi EvaluationService
evaluationReaderServer = evaluateFen :<|> updatePieceWeightings :<|> evaluateFenStream

evalServer :: EvaluationConfig -> Server EvaluationRestApi
evalServer evalConfig = hoistServer evalApiProxy (convertToHandler evalConfig) evaluationReaderServer

createEvalApp :: FilePath -> Maybe FilePath -> IO Application
createEvalApp openingTableSettingsPath evalConfigPath = do
  openingTableSettings <- buildOpeningTable openingTableSettingsPath
  pws <- maybe (pure defaultPieceWeightings) Y.decodeFileThrow evalConfigPath
  pwTvar <- newTVarIO pws
  let evalConfig = EvaluationConfig openingTableSettings pwTvar
  return $ serve evalApiProxy (evalServer evalConfig)

evaluateFenStream :: Stream.Async FenRepresentation -> EvaluationService (Maybe MinAndMaxEval)
evaluateFenStream stream = Stream.foldlM' updateMinAndMaxEval (pure Nothing) evaluationServiceStream
  where
    evaluationServiceStream :: Stream.SerialT EvaluationService FenRepresentation
    evaluationServiceStream = Stream.hoist liftIO (Stream.fromAsync stream)
    updateMinAndMaxEval :: Maybe MinAndMaxEval -> FenRepresentation -> EvaluationService (Maybe MinAndMaxEval)
    updateMinAndMaxEval Nothing fen = do
      evaluation <- evaluateFen fen
      let evaluatedFen = (evaluation, fen)
      return $ Just (MinAndMaxEval evaluatedFen evaluatedFen)
    updateMinAndMaxEval (Just (MinAndMaxEval oldMin@(minEval, _) oldMax@(maxEval, _))) fen = do
      evaluation <- evaluateFen fen
      let newMin =
            if evaluation < minEval
              then (evaluation, fen)
              else oldMin
          newMax =
            if evaluation > maxEval
              then (evaluation, fen)
              else oldMax
      return $ Just (MinAndMaxEval newMin newMax)
