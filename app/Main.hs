{-# LANGUAGE OverloadedStrings #-}

module Main where

import Chess.Fen (startingFenRepresentation)
import Chess.Fen.FenParser
import Chess.OpeningTable.OpeningTable
import Control.Concurrent (threadDelay)
import Control.Monad.Logger
import Control.Monad.Reader (runReaderT)
import Data.Aeson
import Data.ByteString.Lazy.Char8 hiding (putStrLn)
import Database.Persist (insertKey)
import Database.Persist.Postgresql (ConnectionString, SqlPersistT, runMigration, withPostgresqlConn)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import System.Exit

connString :: ConnectionString
connString = "host=postgres port=5432 user=postgres dbname=postgres password=postgres"

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action =
  runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend -> runReaderT action backend

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
  threadDelay 1000000
  runAction connString (runMigration migrateAll)
  let (key, openingPos) = fenWithEvalToOpeningPosition startingFenRepresentation 0.0
  runAction connString (insertKey key openingPos)
