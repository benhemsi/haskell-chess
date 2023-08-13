{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Chess.Evaluation.EvaluationRestApi where

import Chess.Evaluation.PieceWeightings
import Chess.Fen (FenRepresentation)
import Chess.Fen.FenParser (parseFen)
import Control.Arrow (left)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Proxy
import Servant.API

type EvaluationRestApi = EvaluateFenEndpoint :<|> UpdatePieceWeightingsEndpoint

type EvaluateFenEndpoint = "evaluate" :> "fen" :> ReqBody '[ PlainText] FenRepresentation :> Post '[ JSON] Double

type UpdatePieceWeightingsEndpoint
   = "update" :> "piece" :> "weightings" :> ReqBody '[ JSON] (PieceWeightings_ Maybe) :> Post '[ JSON] ()

instance MimeUnrender PlainText FenRepresentation where
  mimeUnrender _ = left show . parseFen . unpack

evalApiProxy :: Proxy EvaluationRestApi
evalApiProxy = Proxy
