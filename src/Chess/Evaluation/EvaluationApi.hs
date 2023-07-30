{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Chess.Evaluation.EvaluationApi where

import Chess.Evaluation.EvaluationConfig
import Chess.Evaluation.EvaluationReader
import Chess.Evaluation.FenEvaluationCalculator (FenEvaluationCalculator, calculateFenEvaluation)
import Chess.Fen (FenRepresentation)
import Chess.Fen.FenParser (parseFen)
import Chess.OpeningTable.OpeningTableAccessor
import Chess.OpeningTable.OpeningTableBuilder (buildOpeningTable)
import Control.Arrow (left)
import Control.Monad.Except
import Control.Monad.Reader
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Proxy
import qualified Data.Yaml as Y
import Servant.API
import Servant.Server

class EvaluationApi m where
  evaluateFen :: FenRepresentation -> m Double

type EvaluationRestApi = "evaluate" :> "fen" :> ReqBody '[ PlainText] FenRepresentation :> Post '[ JSON] Double

instance MimeUnrender PlainText FenRepresentation where
  mimeUnrender _ = left show . parseFen . unpack

evaluateFenWithOpeningTable ::
     (Monad m, OpeningTableAccessor m, FenEvaluationCalculator m) => FenRepresentation -> m Double
evaluateFenWithOpeningTable fen = do
  queriedEvaluation <- lookupFenInOpeningTable fen
  case queriedEvaluation of
    Just eval -> return eval
    Nothing -> calculateFenEvaluation fen

convertToHandler :: EvaluationConfig -> EvaluationReader a -> Handler a
convertToHandler evalConfig evalReader = liftIO ioOutput
  where
    ioOutput = runReaderT (getEvaluationReader evalReader) evalConfig

instance EvaluationApi EvaluationReader where
  evaluateFen = evaluateFenWithOpeningTable

evalApiProxy :: Proxy EvaluationRestApi
evalApiProxy = Proxy

evalServer :: EvaluationConfig -> Server EvaluationRestApi
evalServer evalConfig = hoistServer evalApiProxy (convertToHandler evalConfig) evaluateFen

runEvalServer :: FilePath -> FilePath -> IO Application
runEvalServer openingTableSettingsPath evalConfigPath = do
  openingTableSettings <- buildOpeningTable openingTableSettingsPath
  pieceWeightings <- Y.decodeFileThrow evalConfigPath
  let evalConfig = EvaluationConfig openingTableSettings pieceWeightings
  return $ serve evalApiProxy (evalServer evalConfig)
