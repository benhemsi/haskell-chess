{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Chess.Evaluation.EvaluationApi where

import Chess.Fen (FenRepresentation)
import Chess.Fen.FenParser (parseFen)
import Control.Arrow (left)
import Control.Monad.Trans.Except
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Proxy
import Servant.API
import Servant.Client
import Servant.Server

type EvaluationApi = "evaluate" :> "fen" :> ReqBody '[ JSON] FenRepresentation :> Post '[ JSON] Double
