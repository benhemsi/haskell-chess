{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Chess.Evaluation.EvaluationRestApi where

import Chess.Evaluation.PieceWeightings
import Chess.Evaluation.ServantTypeclassInstances (MinAndMaxEval)
import Chess.Fen (FenRepresentation)
import Data.Proxy
import Servant.API
import qualified Streamly.Prelude as Stream

type EvaluationRestApi = EvaluateFenEndpoint :<|> UpdatePieceWeightingsEndpoint :<|> EvaluationStreamingEndpoint

type EvaluateFenEndpoint = "evaluate" :> "fen" :> ReqBody '[ PlainText] FenRepresentation :> Post '[ JSON] Double

type UpdatePieceWeightingsEndpoint
   = "update" :> "piece" :> "weightings" :> ReqBody '[ JSON] (PieceWeightings_ Maybe) :> Post '[ JSON] PieceWeightings

type EvaluationStreamingEndpoint
   = "evaluate" :> "fens" :> StreamBody NewlineFraming PlainText (Stream.Async FenRepresentation) :> Post '[ JSON] (Maybe MinAndMaxEval)

evalApiProxy :: Proxy EvaluationRestApi
evalApiProxy = Proxy
