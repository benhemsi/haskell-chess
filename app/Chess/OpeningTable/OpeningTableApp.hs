{-# LANGUAGE OverloadedStrings #-}

module Chess.OpeningTable.OpeningTableApp where

import Chess.Fen.FenParser
import Chess.OpeningTable.OpeningTableBuilder
import Control.Concurrent
import Data.Aeson
import Data.ByteString.Lazy.Char8 hiding (putStrLn)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import System.Exit

application req respond =
  if requestMethod req == methodPut
    then do
      body <- lazyRequestBody req
      let parsed = parseFen (unpack body)
      case parsed of
        Right valid -> do
          let json = encode valid
          respond $ responseLBS status200 [("Content-Type", "application/json")] json
        Left error -> do
          respond $ responseLBS status400 [("Content-Type", "text/plain")] (pack (show error))
    else respond $ responseLBS status200 [("Content-Type", "text/plain")] "FEN Parser active"

main = do
  threadDelay (5 * 10 ^ 6)
  output <- buildOpeningTable "./config/opening-table-config.yaml"
  print output
