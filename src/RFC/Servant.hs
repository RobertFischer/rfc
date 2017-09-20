module RFC.Servant
  ( ApiCtx
  , apiCtxToHandler
  , idAndsToMap
  , IdAnd(..)
  , ResourceDefinition(..)
  , ServerAPI
  , ServerImpl
  , module Servant
  , module Servant.Docs
  , module Servant.HTML.Blaze
  , module Text.Blaze.Html
  , module Data.Swagger
  ) where

import RFC.Prelude
import Servant
import qualified RFC.Redis as Redis
import qualified RFC.Psql as Psql
import RFC.JSON
import RFC.Psql
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import Data.HashMap.Lazy as HashMap
import Data.UUID.Types as UUID
import Data.Map as Map
import Data.List as List hiding ((++))
import Servant.Docs hiding (API)
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html
import Data.Swagger (Swagger, ToSchema)

type ApiCtx = ReaderT Psql.ConnectionPool
  ( ReaderT Redis.ConnectionPool
    Handler
  )

instance Psql.HasPsql ApiCtx where
  getPsqlPool = ask

instance Redis.HasRedis ApiCtx where
  getRedisPool = lift ask

apiCtxToHandler :: Redis.ConnectionPool -> Psql.ConnectionPool -> ApiCtx :~> Handler
apiCtxToHandler redisPool psqlPool = NT toHandler
  where
    toHandler :: forall a. ApiCtx a -> Handler a
    toHandler a = withRedis $ withPsql a
      where
        withRedis m = runReaderT m redisPool
        withPsql m = runReaderT m psqlPool

-- |Represents something which has an ID.
newtype IdAnd a = IdAnd (UUID, a)
  deriving (Eq, Ord, Read, Show, Generic, Typeable)

instance (ToSchema a) => ToSchema (IdAnd a)

valuesToIdAnd :: UUID -> a -> IdAnd a
valuesToIdAnd id a = IdAnd (id, a)

idAndToTuple :: IdAnd a -> (UUID, a)
idAndToTuple (IdAnd it) = it

idAndsToMap :: [IdAnd a] -> Map UUID a
idAndsToMap list = Map.fromList $ List.map idAndToTuple list

instance (FromJSON a) => FromJSON (IdAnd a) where
  parseJSON (Object v) = valuesToIdAnd
    <$> v .: "id"
    <*> v .: "value"

  parseJSON invalid = JSON.typeMismatch "IdAnd" invalid


instance (ToJSON a) => ToJSON (IdAnd a) where
  toJSON (IdAnd (uuid, a)) = JSON.Object map
    where
      map = HashMap.union idMap aMap
      idMap = HashMap.singleton "id" uuidStr
      uuidStr = String $ cs $ UUID.toString uuid
      aMap = HashMap.singleton "value" aJson
      aJson = toJSON a

instance (FromRow a) => FromRow (IdAnd a) where
  fromRow = valuesToIdAnd <$> field <*> fromRow

instance (ToRow a) => ToRow (IdAnd a) where
  toRow (IdAnd (id,a)) = toField id : toRow a

instance (FromRow UUID) where
  fromRow = uuidFromString <$> field
    where
      uuidFromString str = fromMaybe UUID.nil $ UUID.fromString str

instance (ToRow UUID) where
  toRow uuid = [toField (toString uuid)]

type FetchAllImpl a = ApiCtx (Map UUID a)
type FetchAllAPI a = Get '[JSON] (Map UUID a)
type FetchImpl a = UUID -> ApiCtx (IdAnd a)
type FetchAPI a = Capture "id" UUID :> Get '[JSON] (IdAnd a)
type CreateImpl a = a -> ApiCtx (IdAnd a)
type CreateAPI a = ReqBody '[JSON] a :> Post '[JSON] (IdAnd a)

type ServerImpl a =
  (FetchAllImpl a)
  :<|> (FetchImpl a)
  :<|> (CreateImpl a)
type ServerAPI a =
  (FetchAllAPI a)
  :<|> (FetchAPI a)
  :<|> (CreateAPI a)


class (FromJSON a, ToJSON a) => ResourceDefinition a where
  restFetchAll :: FetchAllImpl a
  restFetchAll = Map.fromList <$> List.map idAndToTuple <$> fetchAllResources

  restFetch :: FetchImpl a
  restFetch uuid = do
    maybeResource <- fetchResource uuid
    case maybeResource of
      Nothing -> throwError $ err404
        { errReasonPhrase = "No resource found for id"
        , errBody = cs $ "Could not find a resource with UUID: " ++ show uuid
        }
      Just r -> return r

  restCreate :: CreateImpl a
  restCreate a = do
    (IdAnd (id,_)) <- createResource a
    restFetch id

  restServer :: ServerImpl a
  restServer = restFetchAll :<|> restFetch :<|> restCreate

  fetchResource :: UUID -> ApiCtx (Maybe (IdAnd a))
  fetchAllResources :: ApiCtx [IdAnd a]
  createResource :: a -> ApiCtx (IdAnd a)

