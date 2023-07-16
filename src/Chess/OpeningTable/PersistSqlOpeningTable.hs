module Chess.OpeningTable.PersistSqlOpeningTable where

import Control.Monad.Logger
import Database.Persist.Postgresql

newtype PersistSqlOpeningTable a =
  PersistSqlOpeningTable
    { unPersist :: SqlPersistT (LoggingT IO) a
    }

runAction :: ConnectionString -> PersistSqlOpeningTable a -> IO a
runAction connString (PersistSqlOpeningTable action) =
  runStdoutLoggingT $ withPostgresqlConn connString $ \backend -> runSqlConn action backend
