{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

module RFC.Psql
  ( module Database.PostgreSQL.Simple
  , module Database.PostgreSQL.Simple.FromField
  , module Database.PostgreSQL.Simple.FromRow
  , module Database.PostgreSQL.Simple.ToField
  , module Database.PostgreSQL.Simple.ToRow
  , module Database.PostgreSQL.Simple.SqlQQ
  , module Database.PostgreSQL.Simple.Types
  , module RFC.Psql
  ) where

import           Control.Monad.Trans.Control
import           Data.Pool
import           Database.PostgreSQL.Simple           (ConnectInfo, Connection,
                                                       FromRow, In (..),
                                                       Only (..), Query, commit,
                                                       connectDatabase,
                                                       connectPassword,
                                                       connectUser)
import qualified Database.PostgreSQL.Simple           as Psql
import           Database.PostgreSQL.Simple.FromField hiding (Binary)
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.Types
import           RFC.Env
import           RFC.Log
import           RFC.Prelude

type ConnectionPool = Pool Connection

class (MonadIO m, MonadBaseControl IO m) => HasPsql m where
  getPsqlPool :: m ConnectionPool

  withPsqlConnection :: (Connection -> m a) -> m a
  withPsqlConnection action = do
    pool <- getPsqlPool
    withResource pool action

  withPsqlTransaction :: (Connection -> m a) -> m a
  withPsqlTransaction action = withPsqlConnection $ \conn ->
    (liftBaseOp_ (Psql.withTransaction conn)) (action conn)

defaultConnectInfo :: (MonadIO m) => m ConnectInfo
defaultConnectInfo = RFC.Env.readPsqlConnectInfo

createConnectionPool :: (MonadIO m) => ConnectInfo -> m ConnectionPool
createConnectionPool connInfo = liftIO $
    createPool connect close 1 10 100
  where
    connect = do
      logDebug "Opening DB connection"
      Psql.connect connInfo
    close conn = do
      logDebug "Closing DB connection"
      Psql.close conn

query :: (MonadIO m, FromRow r, ToRow q) => Connection -> Query -> q -> m [r]
query conn qry q = liftIO $ Psql.query conn qry q

query_ :: (MonadIO m, FromRow r) => Connection -> Query -> m [r]
query_ conn qry = liftIO $ Psql.query_ conn qry

query1 :: (MonadIO m, FromRow r, ToRow q) => Connection -> Query -> q -> m (Maybe r)
query1 conn qry q = do
  result <- query conn qry q
  return $ case result of
    []    -> Nothing
    (r:_) -> Just r

query1_ :: (MonadIO m, FromRow r) => Connection -> Query -> m (Maybe r)
query1_ conn qry = do
  result <- query_ conn qry
  return $ case result of
    []    -> Nothing
    (r:_) -> Just r

query1Else :: (MonadIO m, FromRow r, ToRow q, Exception e) => Connection -> Query -> q -> e -> m (Maybe r)
query1Else conn qry q e = do
  result <- query1 conn qry q
  case result of
    (Just _) -> return result
    Nothing  -> liftIO $ throwIO e

query1Else_ :: (MonadIO m, FromRow r, Exception e) => Connection -> Query -> e -> m (Maybe r)
query1Else_ conn qry e = do
  result <- query1_ conn qry
  case result of
    (Just _) -> return result
    Nothing  -> liftIO $ throwIO e

execute :: (MonadIO m, ToRow q) => Connection -> Query -> q -> m Int64
execute conn qry q = liftIO $ Psql.execute conn qry q

execute_ :: (MonadIO m) => Connection -> Query -> m Int64
execute_ conn qry = liftIO $ Psql.execute_ conn qry

executeMany :: (MonadIO m, ToRow q) => Connection -> Query -> [q] -> m Int64
executeMany conn qry qs = liftIO $ Psql.executeMany conn qry qs
