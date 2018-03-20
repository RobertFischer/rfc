{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module RFC.Psql
  ( module Database.PostgreSQL.Typed
  , module Database.PostgreSQL.Typed.Query
  , module Database.PostgreSQL.Typed.Types
  , module RFC.Psql
  ) where

import           Data.Pool
import           Database.PostgreSQL.Typed
import           Database.PostgreSQL.Typed.Query
import           Database.PostgreSQL.Typed.Types
import qualified RFC.Env                         as Env
import           RFC.Prelude

type PGConnectionPool = Pool PGConnection
type ConnectionPool = PGConnectionPool

class HasPsql m where
  getPsqlPool :: m PGConnectionPool

instance {-# OVERLAPS #-} (Monad m) => HasPsql (ReaderT PGConnectionPool m) where
  getPsqlPool = ask
  {-# INLINE getPsqlPool #-}

withPsqlConnection :: (HasPsql m, MonadIO m) => (PGConnection -> IO a) -> m a
withPsqlConnection action = do
  pool <- getPsqlPool
  liftIO $ withResource pool action
{-# INLINE withPsqlConnection #-}

withPsqlTransaction :: (HasPsql m, MonadIO m) => IO a -> m a
withPsqlTransaction action = withPsqlConnection $ \conn -> do
  let newMonad = ReaderT (const action)
  liftIO $ pgTransaction conn $ runReaderT newMonad conn
{-# INLINABLE withPsqlTransaction #-}

instance {-# OVERLAPPING #-} Env.DefConfig PGDatabase where
  defConfig = defaultPGDatabase
  {-# INLINE defConfig #-}

instance Env.FromEnv PGDatabase where
  fromEnv = PGDatabase <$> Env.env "PSQL_HOST"
                       <*> Env.env "PSQL_PORT"
                       <*> Env.env "PSQL_DATABASE"
                       <*> Env.env "PSQL_USERNAME"
                       <*> Env.env "PSQL_PASSWORD"
                       <*> pure []
                       <*> pure Env.isDevelopment
                       <*> pure (
                        if Env.isDevelopment then
                          print . PGError
                        else
                          const $ return ()
                       )
  {-# INLINE fromEnv #-}


defaultConnectInfo :: (MonadIO m, MonadFail m) => m PGDatabase
defaultConnectInfo = do
  result <- liftIO $ Env.decodeEnv
  case result of
    Left err       -> fail $ "Could not retrieve psql connection info: " ++ err
    Right connInfo -> return connInfo
{-# INLINE defaultConnectInfo #-}

createConnectionPool :: (MonadIO m) => PGDatabase -> m PGConnectionPool
createConnectionPool connInfo = liftIO $
    createPool connect close 1 10 100
  where
    connect = pgConnect connInfo
    close = pgDisconnect
{-# INLINE createConnectionPool #-}

query :: (MonadIO m, HasPsql m, PGQuery q a) => q -> m [a]
query q = withPsqlConnection $ \conn -> pgQuery conn q
{-# INLINE query #-}

query1 :: (MonadIO m, HasPsql m, PGQuery q a) => q -> m (Maybe a)
query1 qry = safeHead <$> query qry
{-# INLINE query1 #-}

query1Else :: (MonadIO m, HasPsql m, PGQuery q a, Exception e) => q -> e -> m (Maybe a)
query1Else qry e = do
  result <- query1 qry
  case result of
    (Just _) -> return result
    Nothing  -> throwIO e
{-# INLINE query1Else #-}

execute :: (MonadIO m, HasPsql m, PGQuery q ()) => q -> m Int
execute q = withPsqlConnection $ \conn -> pgExecute conn q
{-# INLINE execute #-}

execute_ :: (MonadIO m, HasPsql m, PGQuery q ()) => q -> m ()
execute_ q = do
  _ <- withPsqlConnection $ \conn -> pgExecute conn q
  return ()
{-# INLINE execute_ #-}
