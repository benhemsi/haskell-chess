{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chess.OpeningTable.OpeningTableReader where

import Chess.OpeningTable.OpeningTableSettings
import Chess.OpeningTable.PersistSqlOpeningTable
import Control.Monad.Reader
import Database.Persist.Postgresql (SqlPersistT)

newtype OpeningTableReader a =
  OpeningTableReader
    { unRead :: ReaderT OpeningTableSettings IO a
    }
  deriving (Functor, Applicative, Monad)

liftPersistSql :: PersistSqlOpeningTable a -> OpeningTableReader a
liftPersistSql action = OpeningTableReader output
  where
    output = do
      connString <- asks getConnectionString
      let ioOutput = runAction connString action
      lift ioOutput
