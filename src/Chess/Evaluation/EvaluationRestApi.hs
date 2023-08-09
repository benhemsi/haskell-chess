{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Chess.Evaluation.EvaluationRestApi where

import Chess.Fen (FenRepresentation)
import Chess.Fen.FenParser (parseFen)
import Control.Arrow (left)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Proxy
import Servant.API

type EvaluationRestApi
   = "evaluate" :> "fen" :> ReqBody '[ PlainText] FenRepresentation :> Post '[ JSON] Double :<|> "evaluation" :> "service" :> "health" :> Get '[ PlainText] String

instance MimeUnrender PlainText FenRepresentation where
  mimeUnrender _ = left show . parseFen . unpack

evalApiProxy :: Proxy EvaluationRestApi
evalApiProxy = Proxy
