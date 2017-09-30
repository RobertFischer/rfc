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
import RFC.HTTP.Client
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
import Network.Wreq.Session as Wreq
import qualified Data.Aeson.Diff as JSON
import Database.PostgreSQL.Simple (SqlError(..))

type ApiCtx =
  ReaderT Wreq.Session
    ( ReaderT Psql.ConnectionPool
      ( ReaderT Redis.ConnectionPool
        Handler
      )
    )

instance HasAPIClient ApiCtx where
  getAPIClient = ask

instance Psql.HasPsql ApiCtx where
  getPsqlPool = lift ask

instance Redis.HasRedis ApiCtx where
  getRedisPool = lift $ lift ask

apiCtxToHandler :: Wreq.Session -> Redis.ConnectionPool -> Psql.ConnectionPool -> ApiCtx :~> Handler
apiCtxToHandler apiClient redisPool psqlPool = NT toHandler
  where
    toHandler :: forall a. ApiCtx a -> Handler a
    toHandler a = withRedis $ withPsql $ withAPIClient a
      where
        withAPIClient m = runReaderT m apiClient
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
type PatchImpl a = UUID -> JSON.Patch -> ApiCtx (IdAnd a)
type PatchAPI a = Capture "id" UUID :> ReqBody '[JSON] JSON.Patch :> Patch '[JSON] (IdAnd a)
type ReplaceImpl a = UUID -> a -> ApiCtx (IdAnd a)
type ReplaceAPI a = Capture "id" UUID :> ReqBody '[JSON] a :> Post '[JSON] (IdAnd a)

type ServerImpl a =
  (FetchAllImpl a)
  :<|> (FetchImpl a)
  :<|> (CreateImpl a)
  :<|> (PatchImpl a)
  :<|> (ReplaceImpl a)
type ServerAPI a =
  (FetchAllAPI a)
  :<|> (FetchAPI a)
  :<|> (CreateAPI a)
  :<|> (PatchAPI a)
  :<|> (ReplaceAPI a)


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
      Just value -> return $ IdAnd (uuid, value)

  restCreate :: CreateImpl a
  restCreate a = handleDupes $ do
      id <- createResource a
      restFetch id

  restPatch :: PatchImpl a
  restPatch id patch = handleDupes $ do
    (IdAnd (_,original::a)) <- restFetch id
    case JSON.patch patch $ toJSON original of
      Error str -> throwError $ err400
        { errReasonPhrase = "Error applying patch"
        , errBody = cs str
        }
      Success jsonValue ->
        case JSON.eitherDecode' $ JSON.encode jsonValue of
          Left err -> throwError $ err400
            { errReasonPhrase = "Error rebuilding object after patch"
            , errBody = cs err
            }
          Right value -> restReplace id value

  restReplace :: ReplaceImpl a
  restReplace id value = handleDupes $ do
      replaceResource newValue
      return newValue
    where
      newValue = IdAnd (id,value)

  restServer :: ServerImpl a
  restServer = restFetchAll :<|> restFetch :<|> restCreate :<|> restPatch :<|> restReplace

  fetchResource :: UUID -> ApiCtx (Maybe a)
  fetchAllResources :: ApiCtx [IdAnd a]
  createResource :: a -> ApiCtx UUID
  replaceResource :: (IdAnd a) -> ApiCtx ()

handleDupes :: ApiCtx a -> ApiCtx a
handleDupes =
    handleJust isDuplicate throwUp
  where
    throwUp err = throwError $ err409
      { errReasonPhrase = cs $ sqlErrorMsg err
      , errBody = cs $ sqlErrorDetail err
      }
    isDuplicate (sqle::SqlError)
      | sqlState sqle  == "23505" = Just sqle
    isDuplicate _ = Nothing

