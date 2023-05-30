{-# LANGUAGE OverloadedStrings #-}

module Chess.OpeningTable.OpeningTablePostgres where

import Database.Persist.Postgresql (ConnectionString)

connString :: ConnectionString
connString = "host=127.0.0.1 port=5432 user=postgres dbname=postgres password=password"
