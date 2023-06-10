{-# LANGUAGE OverloadedStrings #-}

module Main where

import Chess.Fen.FenParser
import Data.Aeson
import Data.ByteString.Lazy.Char8
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

main = run 3000 application
