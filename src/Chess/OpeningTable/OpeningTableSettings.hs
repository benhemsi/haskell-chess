{-# LANGUAGE DeriveGeneric #-}

module Chess.OpeningTable.OpeningTableSettings where

import Data.Aeson (FromJSON)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable
import qualified Data.Yaml as Y
import Database.Persist.Postgresql (ConnectionString)
import GHC.Generics

data OpeningTableSettings =
  OpeningTableSettings
    { host, user, dbname, password :: String
    , port :: Int
    }
  deriving (Generic)

instance FromJSON OpeningTableSettings

getConnectionString :: OpeningTableSettings -> ConnectionString
getConnectionString settings = BS.pack connString
  where
    connStringParts =
      [ "host="
      , host settings
      , " port="
      , show $ port settings
      , " user="
      , user settings
      , " dbname="
      , dbname settings
      , " password="
      , password settings
      ]
    connString :: String
    connString = fold connStringParts
