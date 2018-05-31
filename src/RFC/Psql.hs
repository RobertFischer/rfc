{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module RFC.Psql
  ( module Database.PostgreSQL.Typed
  , module Database.PostgreSQL.Typed.Array
  , module Database.PostgreSQL.Typed.Query
  , module Database.PostgreSQL.Typed.Types
  , module Database.PostgreSQL.Typed.TH
  , module RFC.Psql
  , module Network
  ) where

import           Control.Monad.Trans.Class       ( MonadTrans (..) )
import           Control.Monad.Trans.Reader      ( ask )
import qualified Data.ByteString.Char8           as C8
import           Data.Pool
import           Database.PostgreSQL.Typed
import           Database.PostgreSQL.Typed.Array
import           Database.PostgreSQL.Typed.Query
import           Database.PostgreSQL.Typed.TH
import           Database.PostgreSQL.Typed.Types
import           Network                         ( PortID (PortNumber) )
import qualified RFC.Data.UUID                   as UUID
import qualified RFC.Env                         as Env
import           RFC.Prelude                     hiding ( ask )

type PGConnectionPool = Pool PGConnection
type ConnectionPool = PGConnectionPool

class HasPsql m where
  withPsqlConnection :: (PGConnection -> IO a) -> m a

instance {-# OVERLAPPABLE #-} (MonadTrans t, Monad m, HasPsql m) => HasPsql (t m) where
  withPsqlConnection :: (PGConnection -> IO a) -> (t m) a
  withPsqlConnection = lift . withPsqlConnection

instance {-# OVERLAPS #-} (MonadIO m) => HasPsql (ReaderT PGConnectionPool m) where
  withPsqlConnection :: (PGConnection -> IO a) -> ReaderT PGConnectionPool m a
  withPsqlConnection action = do
    pool <- ask
    liftIO $ withResource pool action
  {-# INLINE withPsqlConnection #-}
  {-# SPECIALIZE instance HasPsql (ReaderT PGConnectionPool IO) #-}

instance {-# OVERLAPS #-} (MonadIO m) => HasPsql (ReaderT PGConnection m) where
  withPsqlConnection :: (PGConnection -> IO a) -> ReaderT PGConnection m a
  withPsqlConnection action = do
    conn <- ask
    liftIO $ action conn
  {-# INLINE withPsqlConnection #-}
  {-# SPECIALIZE instance HasPsql (ReaderT PGConnection IO) #-}

liftHasPsql :: (PGConnection -> IO a) -> ReaderT PGConnection IO a
liftHasPsql = ReaderT
{-# INLINE liftHasPsql #-}

withPsqlTransaction :: (HasPsql psql) => (PGConnection -> IO a) -> psql a
withPsqlTransaction action = withPsqlConnection $ \conn ->
    pgTransaction conn (action conn)
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
  result <- liftIO Env.decodeEnv
  case result of
    Left err       -> fail $ "Could not retrieve psql connection info: " <> err
    Right connInfo -> return connInfo
{-# INLINE defaultConnectInfo #-}

createConnectionPool :: (MonadIO m) => PGDatabase -> m PGConnectionPool
createConnectionPool connInfo = liftIO $
    createPool connect close 1 10 100
  where
    connect = pgConnect connInfo
    close = pgDisconnect
{-# INLINE createConnectionPool #-}

query :: (HasPsql m, PGQuery q a) => q -> m [a]
query q = withPsqlConnection $ \conn -> liftIO $ pgQuery conn q
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

execute :: (HasPsql m, PGQuery q ()) => q -> m Int
execute q = withPsqlConnection $ \conn -> liftIO $ pgExecute conn q
{-# INLINE execute #-}

execute_ :: (MonadIO m, HasPsql m, PGQuery q ()) => q -> m ()
execute_ = void . execute
{-# INLINE execute_ #-}

instance {-# OVERLAPPING #-} PGType "style" where
  type PGVal "style" = StrictText
  pgBinaryColumn _ _ = True

instance {-# OVERLAPPING #-} PGStringType "style"

instance {-# OVERLAPPING #-} PGColumn "uuid[]" [UUID] where
  pgDecode _ sbs =
      let (sbsList::[StrictByteString]) = C8.split (pgArrayDelim pgTypePxy) sbs in
      let strList = catMaybes $ toUTF8 <$> sbsList in
      let uuidList = catMaybes $ UUID.fromString <$> strList in
      uuidList
    where
      pgTypePxy = PGTypeProxy :: PGTypeID "uuid[]"

