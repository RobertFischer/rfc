{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module RFC.Servant
  ( ApiCtx
  , apiCtxToHandler
  , ResourceDefinition(..)
  , ServerAPI
  , ServerImpl
  , module Servant
  , module Servant.Docs
  , module Servant.HTML.Blaze
  , module Text.Blaze.Html
  , module RFC.Data.IdAnd
  , module RFC.API
  ) where

import           Control.Monad.Trans.AWS
import           Control.Natural         (type (~>))
import           Data.Aeson              as JSON
import           Network.AWS             as AWS
import           Network.Wreq.Session    as Wreq
import           RFC.API
import           RFC.Data.IdAnd
import           RFC.HTTP.Client
import           RFC.JSON                ()
import           RFC.Prelude             hiding (Handler)
import           RFC.Prelude.Instances   ()
import qualified RFC.Psql                as Psql
import qualified RFC.Redis               as Redis
import           Servant
import           Servant.Docs            hiding (API)
import           Servant.HTML.Blaze      (HTML)
import           Servant.Server          (Handler, runHandler)
import           Text.Blaze.Html

type ApiCtx =
  AWST
    ( ReaderT Wreq.Session
      ( ReaderT Psql.ConnectionPool
        ( ReaderT Redis.ConnectionPool
          Handler
        )
      )
    )

instance MonadUnliftIO Handler where
  withRunInIO iobFactory = liftIO $ iobFactory converter
    where
      converter h = do
        eitherT <- runHandler h
        case eitherT of
          Left err -> throwIO err
          Right a  -> return a
  {-# INLINE withRunInIO #-}

instance HasAPIClient ApiCtx where
  getAPIClient = lift ask
  {-# INLINE getAPIClient #-}

instance (Monad m) => HasAPIClient (ReaderT Wreq.Session m) where
  getAPIClient = ask
  {-# INLINE getAPIClient #-}

instance Redis.HasRedis ApiCtx where
  getRedisPool = lift . lift $ lift ask
  {-# INLINE getRedisPool #-}

apiCtxToHandler :: Wreq.Session -> Redis.ConnectionPool -> Psql.ConnectionPool -> AWS.Env -> ApiCtx ~> Handler
apiCtxToHandler apiClient redisPool psqlPool awsEnv = toHandler
  where
    toHandler :: ApiCtx ~> Handler
    toHandler = withRedis . withPsql . withAPIClient . withAws
      where
        withAPIClient m = runReaderT m apiClient
        withRedis m = runReaderT m redisPool
        withPsql m = runReaderT m psqlPool
        withAws = runAWST awsEnv
{-# INLINE apiCtxToHandler #-}

type FetchAllImpl a = ApiCtx (RefMap a)
type FetchAllAPI a = JGet (RefMap a)
type FetchImpl a = UUID -> ApiCtx (IdAnd a)
type FetchAPI a = Capture "id" UUID :> JGet (IdAnd a)
type CreateImpl a = a -> ApiCtx (IdAnd a)
type CreateAPI a = JReqBody a :> JPost (IdAnd a)
--type PatchImpl a = UUID -> JSON.Patch -> ApiCtx (IdAnd a)
--type PatchAPI a = Capture "id" UUID :> ReqBody '[JSON] JSON.Patch :> Patch '[JSON] (IdAnd a)
type ReplaceImpl a = UUID -> a -> ApiCtx (IdAnd a)
type ReplaceAPI a = Capture "id" UUID :> JReqBody a :> JPost (IdAnd a)
type DeleteImpl = UUID -> ApiCtx ()
type DeleteAPI a = Capture "id" UUID :> JDelete ()

type ServerImpl a =
  (FetchAllImpl a)
  :<|> (FetchImpl a)
  :<|> (CreateImpl a)
  -- :<|> (PatchImpl a)
  :<|> (ReplaceImpl a)
  :<|> DeleteImpl
type ServerAPI a =
  (FetchAllAPI a)
  :<|> (FetchAPI a)
  :<|> (CreateAPI a)
  -- :<|> (PatchAPI a)
  :<|> (ReplaceAPI a)
  :<|> (DeleteAPI a)


class (FromJSON a, ToJSON a, Show a) => ResourceDefinition a where

  -- | Provide all UUID of all the children of this resource.
  --   A "child" of a resource "parent" is defined as another
  --   resource which is copied when the parent is copied.
  resourceChildIds :: Proxy a -> UUID -> ApiCtx [UUID]

  restFetchAll :: FetchAllImpl a
  restFetchAll = idAndsToMap <$> fetchAllResources
  {-# INLINE restFetchAll #-}

  restFetch :: FetchImpl a
  restFetch uuid = do
    maybeResource <- fetchResource uuid
    case maybeResource of
      Nothing -> throwError $ err404
        { errReasonPhrase = "No resource found for id"
        , errBody = asUTF8 $ "Could not find a resource with UUID: " <> show uuid
        }
      Just value -> return $ IdAnd (uuid, value)
  {-# INLINE restFetch #-}

  restCreate :: CreateImpl a
  restCreate a = do
      maybeId <- createResource a
      case maybeId of
        (Just id) -> restFetch id
        Nothing -> throwIO $ err400
          { errReasonPhrase = "Could not create resource"
          , errBody = asUTF8 $ show a
          }
  {-# INLINE restCreate #-}

{-
  restPatch :: PatchImpl a
  restPatch id patch = do
    (IdAnd (_,original::a)) <- restFetch id
    case JSON.patch patch $ toJSON original of
      Error str -> throwError $ err400
        { errReasonPhrase = "Error applying patch"
        , errBody = asUTF8 str
        }
      Success jsonValue ->
        case JSON.eitherDecode' $ JSON.encode jsonValue of
          Left err -> throwError $ err400
            { errReasonPhrase = "Error rebuilding object after patch"
            , errBody = asUTF8 err
            }
          Right value -> restReplace id value
  {-# INLINE restPatch #-}
-}

  restReplace :: ReplaceImpl a
  restReplace id value = do
      replaceResource newValue
      restFetch id
    where
      newValue = IdAnd (id,value)
  {-# INLINE restReplace #-}

  restDelete :: Proxy a -> DeleteImpl
  restDelete = deleteResource

  restServer :: ServerImpl a
  restServer =
    restFetchAll
    :<|> restFetch
    :<|> restCreate
    -- :<|> restPatch
    :<|> restReplace
    :<|> (restDelete (Proxy::Proxy a))

  fetchResource :: UUID -> ApiCtx (Maybe a)
  fetchAllResources :: ApiCtx [IdAnd a]
  createResource :: a -> ApiCtx (Maybe UUID)
  replaceResource :: (IdAnd a) -> ApiCtx ()
  deleteResource :: Proxy a -> UUID -> ApiCtx ()
