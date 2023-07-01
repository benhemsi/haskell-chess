{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Chess.OpeningTable.OpeningTableApi where

import Chess.Fen (FenRepresentation)
import Chess.Fen.FenParser (parseFen)
import Chess.OpeningTable.OpeningTable (getFenEvaluation)
import Chess.OpeningTable.OpeningTablePostgres
import Control.Arrow (left)
import Control.Monad.Trans.Except
import Data.Proxy
import Data.Text (pack, unpack)
import Servant.API
import Servant.Client
import Servant.Server

type OpeningTableApi = "opening" :> "table" :> Capture "position" FenRepresentation :> Get '[ JSON] (Maybe Double)

openingPositionHandler :: FenRepresentation -> Handler (Maybe Double)
openingPositionHandler fen =
  let lookup :: IO (Maybe Double)
      lookup = runAction connString (getFenEvaluation fen)
      output = fmap Right lookup
   in Handler (ExceptT output)

openingTableServer :: Server OpeningTableApi
openingTableServer = openingPositionHandler

instance FromHttpApiData FenRepresentation where
  parseUrlPiece = left (pack . show) . parseFen . unpack

instance ToHttpApiData FenRepresentation where
  toUrlPiece = pack . show

openingTableApi :: Proxy OpeningTableApi
openingTableApi = Proxy

getOpeningPositionEvaluation :: FenRepresentation -> ClientM (Maybe Double)
getOpeningPositionEvaluation = client openingTableApi
