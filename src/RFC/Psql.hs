module RFC.Psql
  ( module Database.PostgreSQL.Simple
  , module RFC.Psql
  ) where

import RFC.Prelude
import RFC.String
import Database.PostgreSQL.Simple (Connection, connectUser, connectPassword, connectDatabase, ConnectInfo)
import qualified Database.PostgreSQL.Simple as Psql
import Data.Pool
import Control.Monad.Trans.Control

type ConnectionPool = Pool Connection

class (MonadIO m, MonadCatch m, MonadBaseControl IO m) => HasPsql m where
  getPsqlPool :: m ConnectionPool

  withPsqlConnection :: (Connection -> m a) -> m a
  withPsqlConnection action = do
    pool <- getPsqlPool
    withResource pool action

  withPsqlTransaction :: (Connection -> m a) -> m a
  withPsqlTransaction action = withPsqlConnection $ \conn ->
    (liftBaseOp_ (Psql.withTransaction conn)) (action conn)

defaultConnectInfo :: (ConvertibleStrings a String) => a -> ConnectInfo
defaultConnectInfo rawProjectThunk = Psql.defaultConnectInfo
    { connectUser = projectThunk
    , connectPassword = projectThunk
    , connectDatabase = projectThunk
    }
  where
    projectThunk :: String
    projectThunk = cs rawProjectThunk

createConnectionPool :: (MonadIO m) => ConnectInfo -> m ConnectionPool
createConnectionPool connInfo = liftIO $
    createPool connect close 1 10 100
  where -- These are left here because we may want to log sometime
    connect = Psql.connect connInfo
    close = Psql.close

