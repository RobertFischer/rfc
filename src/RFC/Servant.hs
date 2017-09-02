module RFC.Servant
  ( ApiCtx
  , apiCtxToHandler
  ) where

import RFC.Prelude
import Servant
import qualified RFC.Redis as Redis
import qualified RFC.Psql as Psql

type ApiCtx = ReaderT Psql.ConnectionPool
  ( ReaderT Redis.ConnectionPool
    Handler
  )

instance Psql.HasPsql BeeWell where
  getPsqlPool = ask

instance Redis.HasRedis BeeWell where
  getRedisPool = lift ask

apiCtxToHandler :: Redis.ConnectionPool -> Psql.ConnectionPool -> ApiCtx :~> Handler
apiCtxToHandler redisPool psqlPool = NT toHandler
  where
    toHandler :: forall a. ApiCtx a -> Handler a
    toHandler a = withRedis $ withPsql a
      where
        withRedis m = runReaderT m redisPool
        withPsql m = runReaderT m psqlPool
