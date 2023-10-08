{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Chess.Evaluation.EvaluationClient where

import Chess.Evaluation.EvaluationApi
import Chess.Evaluation.EvaluationRestApi
import Chess.Evaluation.PieceWeightings
import Chess.Evaluation.ServantTypeclassInstances
import Chess.Fen (FenRepresentation)
import Control.Monad.Logger
import Control.Monad.Reader
import Network.HTTP.Client
import Servant.API
import Servant.Client
import qualified Streamly.Prelude as Stream
import UnliftIO.Exception

newtype EvaluationClient a =
  EvaluationClient
    { getEvaluationClient :: ReaderT EvaluationClientConfig (LoggingT IO) a
    }
  deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadLoggerIO)

instance EvaluationApi EvaluationClient where
  evaluateFen = postFenEval
  updatePieceWeightings = postPieceWeightings

data EvaluationClientConfig =
  EvaluationClientConfig
    { host :: String
    , port :: Int
    }

makeEvaluationClientEnvFromConfig :: EvaluationClientConfig -> IO ClientEnv
makeEvaluationClientEnvFromConfig (EvaluationClientConfig host port) = clientEnv
  where
    baseUrl = BaseUrl Http host port ""
    manager = newManager defaultManagerSettings
    clientEnv = fmap (`mkClientEnv` baseUrl) manager

convertToClient :: ClientM a -> EvaluationClient a
convertToClient clientM = EvaluationClient output
  where
    output = do
      evalClientConfig <- ask
      clientEnv <- liftIO $ makeEvaluationClientEnvFromConfig evalClientConfig
      fromEitherIO $ runClientM clientM clientEnv

postFenEval :: FenRepresentation -> EvaluationClient Double
postPieceWeightings :: PieceWeightings_ Maybe -> EvaluationClient PieceWeightings
postFens :: Stream.Serial FenRepresentation -> EvaluationClient Double
postFenEval :<|> postPieceWeightings :<|> postFens = hoistClient evalApiProxy convertToClient (client evalApiProxy)
