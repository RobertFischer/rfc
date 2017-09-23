module RFC.Redis
  ( createConnectionPool
  , ConnectionPool
  , HasRedis(..)
  , RedisException(..)
  , get
  , setex
  ) where

import RFC.Prelude
import qualified Database.Redis as R
import Database.Redis
  ( Redis, connect, defaultConnectInfo, Connection )
import RFC.String

type ConnectionPool = Connection

newtype RedisException = RedisException R.Reply deriving (Typeable, Show)
instance Exception RedisException

class (MonadIO m, MonadThrow m) => HasRedis m where
  getRedisPool :: m ConnectionPool

  runRedis :: Redis a -> m a
  runRedis r = do
    conn <- getRedisPool
    liftIO $ R.runRedis conn r

createConnectionPool :: (MonadIO m) => m ConnectionPool
createConnectionPool = liftIO $ connect $ defaultConnectInfo

get :: (HasRedis m, ConvertibleToSBS tIn, ConvertibleFromSBS tOut) => tIn -> m (Maybe tOut)
get key = do
  result <- runRedis $ R.get $ cs key
  maybeResult <- unpack result
  return $ cs <$> maybeResult

unpack :: (MonadThrow m) => Either R.Reply a -> m a
unpack (Left reply) = throw $ RedisException reply
unpack (Right it) = return it

setex :: (HasRedis m, ConvertibleToSBS key, ConvertibleToSBS value, TimeUnit expiry) => key -> value -> expiry -> m ()
setex key value expiry = do
    result <- runRedis $ R.setex (cs key) milliseconds (cs value)
    _ <- unpack result
    return ()
  where
    milliseconds = fromIntegral $ ( (convertUnit expiry)::Millisecond )
