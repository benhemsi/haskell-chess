{-# LANGUAGE OverloadedStrings #-}

module Chess.OpeningTable.OpeningTableApp where

import Chess.Fen (startingFenRepresentation)
import Chess.Fen.FenParser
import Control.Concurrent (threadDelay)
import Control.Monad.Logger
import Control.Monad.Reader (runReaderT)
import Data.Aeson
import Data.ByteString.Lazy.Char8 hiding (putStrLn)
import Database.Persist (insertKey)
import Database.Persist.Postgresql as PS (get, runMigration)
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
-- threadDelay 1000000
-- runAction connString (runMigration migrateAll)
-- let (key, openingPos) = fenWithEvalToOpeningPosition startingFenRepresentation 0.0
-- runAction connString (insertKey key openingPos)
-- eval <- runAction connString (PS.get $ fenToOpeningPositionKey startingFenRepresentation)
-- print $ fmap _openingPositionEvaluation eval
