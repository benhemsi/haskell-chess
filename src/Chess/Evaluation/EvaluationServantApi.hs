{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Chess.Evaluation.EvaluationServantApi where

import Chess.Evaluation.EvaluationApi
import Chess.Evaluation.EvaluationConfig
import Chess.Evaluation.EvaluationReader
import Chess.Fen (FenRepresentation)
import Chess.Fen.FenParser (parseFen)
import Chess.OpeningTable.OpeningTableBuilder (buildOpeningTable)
import Control.Arrow (left)
import Control.Monad.Except
import Control.Monad.Reader
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Proxy
import qualified Data.Yaml as Y
import Servant.API
import Servant.Server

type EvaluationRestApi = "evaluate" :> "fen" :> ReqBody '[ PlainText] FenRepresentation :> Post '[ JSON] Double

instance MimeUnrender PlainText FenRepresentation where
  mimeUnrender _ = left show . parseFen . unpack

convertToHandler :: EvaluationConfig -> EvaluationReader a -> Handler a
convertToHandler evalConfig evalReader = liftIO ioOutput
  where
    ioOutput = runReaderT (getEvaluationReader evalReader) evalConfig

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
