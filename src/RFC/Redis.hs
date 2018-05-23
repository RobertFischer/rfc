{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}


module RFC.Redis
  ( createConnectionPool
  , ConnectionPool
  , HasRedis(..)
  , RedisException(..)
  , get
  , setex
  ) where

import           Data.Time.Clock (nominalDay)
import qualified Database.Redis  as R
import           RFC.Env         as Env
import           RFC.Prelude
import           RFC.String      ()

type ConnectionPool = R.Connection

newtype RedisException = RedisException R.Reply deriving (Typeable, Show)
instance Exception RedisException

class (MonadIO m) => HasRedis m where
  getRedisPool :: m ConnectionPool

  runRedis :: R.Redis a -> m a
  runRedis r = do
    conn <- getRedisPool
    liftIO $ R.runRedis conn r
  {-# INLINE runRedis #-}


instance DefConfig R.ConnectInfo where
  defConfig = R.defaultConnectInfo
  {-# INLINE defConfig #-}

instance FromEnv R.ConnectInfo where
  fromEnv = R.ConnInfo
    <$> envWithDevDefault "REDIS_HOST" (R.connectHost defConfig)
    <*> envWithDefault "REDIS_PORT" (R.connectPort defConfig)
    <*> envWithDefault "REDIS_AUTH" (R.connectAuth defConfig)
    <*> envWithDefault "REDIS_DB" (R.connectDatabase defConfig)
    <*> envWithDefault "REDIS_MAX_CONNS" (R.connectMaxConnections defConfig)
    <*> envWithDefault "REDIS_IDLE_TIMEOUT" (R.connectMaxIdleTime defConfig)
    <*> envWithDefault "REDIS_CONN_TIMEOUT" (Just (nominalDay/24)) -- No timeout by default!
    <*> return Nothing
  {-# INLINABLE fromEnv #-}

createConnectionPool :: (MonadUnliftIO m, MonadFail m) => m ConnectionPool
createConnectionPool = do
  connInfoResult <- liftIO decodeEnv
  case connInfoResult of
    Left err       -> fail $ "Could not configure Redis connection: " <> err
    Right connInfo -> liftIO $ R.connect connInfo

get :: (HasRedis m, ConvertibleToSBS tIn, ConvertibleFromSBS tOut) => tIn -> m (Maybe tOut)
get key = do
  result <- runRedis . R.get $ cs key
  maybeResult <- liftIO $ unpack result
  return $ cs <$> maybeResult

unpack :: (MonadUnliftIO m) => Either R.Reply a -> m a
unpack (Left reply) = throwIO $ RedisException reply
unpack (Right it)   = return it

setex :: (HasRedis m, ConvertibleToSBS key, ConvertibleToSBS value, TimeUnit expiry) => key -> value -> expiry -> m ()
setex key value expiry = do
    result <- runRedis $ R.setex (cs key) milliseconds (cs value)
    _ <- liftIO $ unpack result
    return ()
  where
    milliseconds = fromIntegral ( (convertUnit expiry)::Millisecond )
