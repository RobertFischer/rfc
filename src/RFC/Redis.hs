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

import qualified Database.Redis as R
import           RFC.Env        as Env
import           RFC.Prelude
import           RFC.String

type ConnectionPool = R.Connection

newtype RedisException = RedisException R.Reply deriving (Typeable, Show)
instance Exception RedisException

class (MonadUnliftIO m) => HasRedis m where
  getRedisPool :: m ConnectionPool

  runRedis :: R.Redis a -> m a
  runRedis r = do
    conn <- getRedisPool
    liftIO $ R.runRedis conn r

createConnectionPool :: (MonadUnliftIO m) => m ConnectionPool
createConnectionPool = do
  connInfo <- Env.readRedisConnectInfo
  liftIO $ R.connect connInfo

get :: (HasRedis m, ConvertibleToSBS tIn, ConvertibleFromSBS tOut) => tIn -> m (Maybe tOut)
get key = do
  result <- runRedis $ R.get $ cs key
  maybeResult <- unpack result
  return $ cs <$> maybeResult

unpack :: (MonadUnliftIO m) => Either R.Reply a -> m a
unpack (Left reply) = throwIO $ RedisException reply
unpack (Right it)   = return it

setex :: (HasRedis m, ConvertibleToSBS key, ConvertibleToSBS value, TimeUnit expiry) => key -> value -> expiry -> m ()
setex key value expiry = do
    result <- runRedis $ R.setex (cs key) milliseconds (cs value)
    _ <- unpack result
    return ()
  where
    milliseconds = fromIntegral $ ( (convertUnit expiry)::Millisecond )
