{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Chess.Evaluation.EvaluationClient where

import Chess.Evaluation.EvaluationApi
import Chess.Evaluation.EvaluationServantApi
import Chess.Fen (FenRepresentation)
import Control.Arrow (left)
import Control.Monad.Logger
import Control.Monad.Reader
import Data.ByteString.Lazy.Char8 (pack)
import Network.HTTP.Client
import Servant.API
import Servant.Client
import UnliftIO.Exception

newtype EvaluationClient a =
  EvaluationClient
    { unClient :: ReaderT EvaluationClientConfig (LoggingT IO) a
    }
  deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadLoggerIO)

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

instance MimeRender PlainText FenRepresentation where
  mimeRender _ = pack . show

evaluationClient :: Client EvaluationClient EvaluationRestApi
evaluationClient = undefined

postFenEval :: FenRepresentation -> EvaluationClient Double
postFenEval :<|> _ = hoistClient evalApiProxy convertToClient (client evalApiProxy)

instance EvaluationApi EvaluationClient where
  evaluateFen = postFenEval
