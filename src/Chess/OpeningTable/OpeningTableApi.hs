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
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Proxy
import Servant.API
import Servant.Client
import Servant.Server

type OpeningTableApi = "opening" :> "table" :> ReqBody '[ JSON] FenRepresentation :> Post '[ JSON] (Maybe Double)

-- openingPositionHandler :: FenRepresentation -> Handler (Maybe Double)
-- openingPositionHandler fen =
--   let lookup :: IO (Maybe Double)
--       lookup = runAction connString (getFenEvaluation fen)
--       output = fmap Right lookup
--    in Handler (ExceptT output)
openingPositionHandler :: FenRepresentation -> Handler (Maybe Double)
openingPositionHandler fen = return (Just 1.0)

openingTableServer :: Server OpeningTableApi
openingTableServer = openingPositionHandler

instance MimeUnrender PlainText FenRepresentation where
  mimeUnrender _ = left show . parseFen . unpack

instance MimeRender PlainText FenRepresentation where
  mimeRender _ = pack . show

-- instance FromHttpApiData FenRepresentation where
--   parseUrlPiece = left (pack . show) . parseFen . unpack
-- instance ToHttpApiData FenRepresentation where
--   toUrlPiece = pack . show
openingTableApi :: Proxy OpeningTableApi
openingTableApi = Proxy

getOpeningPositionEvaluation :: FenRepresentation -> ClientM (Maybe Double)
getOpeningPositionEvaluation = client openingTableApi

-- getOpeningPositionEvaluation :: FenRepresentation -> ClientM (Maybe Double)
-- getOpeningPositionEvaluation = client openingTableApi
openingTableApp :: Application
openingTableApp = serve openingTableApi openingTableServer
