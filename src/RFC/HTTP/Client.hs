{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE UndecidableInstances  #-}

module RFC.HTTP.Client
  ( withAPISession
  , HasAPIClient(..)
  , HasHttpManager(..)
  , BadStatusException
  , apiGet
  , webGet
  , module Network.Wreq.Session
  , module Network.HTTP.Types.Status
  ) where

import           Control.Lens
import           Network.HTTP.Client       (Manager, ManagerSettings,
                                            newManager)
import           Network.HTTP.Client.TLS   (tlsManagerSettings)
import           Network.HTTP.Types.Status hiding (statusCode, statusMessage)
import           Network.URI
import           Network.Wreq.Lens
import           Network.Wreq.Session      hiding (withAPISession)
import           RFC.JSON                  (FromJSON, decodeOrDie)
import           RFC.Prelude
import           RFC.String                ()

rfcManagerSettings :: ManagerSettings
rfcManagerSettings = tlsManagerSettings

createRfcManager :: (MonadIO m) => m Manager
createRfcManager = liftIO $ newManager rfcManagerSettings

withAPISession :: (MonadIO m) => (Session -> m a) -> m a
withAPISession = (>>=) $ liftIO (newSessionControl Nothing rfcManagerSettings)

newtype BadStatusException = BadStatusException (Status,URI)
  deriving (Show,Eq,Ord,Generic,Typeable)
instance Exception BadStatusException

apiExecute :: (HasAPIClient m, MonadUnliftIO m, ConvertibleString LazyByteString s)  =>
  URI -> (Session -> String -> IO (Response LazyByteString)) -> (s -> m a) -> m a
apiExecute rawUrl action converter =
  converter . cs <$> webExecute rawUrl action

webExecute :: (HasAPIClient m, MonadUnliftIO m, Exception e) =>
  URI -> (Session -> String -> IO (Response LazyByteString)) -> m LazyByteString
webExecute rawUrl action = handle onError $ do
  session <- getAPIClient
  response <- liftIO $ action session url
  let status = response ^. responseStatus
  case status ^. statusCode of
    200 -> return $ response ^. responseBody
    _   -> throwIO $ badResponseStatus status
  where
    url = show rawUrl
    badResponseStatus status = BadStatusException (status, rawUrl)

webGet :: (HasAPIClient m, MonadUnliftIO m, Exception e) => URI -> (e -> m a) -> m a
webGet url onError =
  handle onError $ webExecute url get

apiGet :: (HasAPIClient m, FromJSON a, MonadUnliftIO m, Exception e) => URI -> (e -> m a) -> m a
apiGet url onError =
  handle onError $ apiExecute url get decodeOrDie

class HasAPIClient m where
  getAPIClient :: m Session

class HasHttpManager m where
  getHttpManager :: m Manager

instance (MonadIO m) => HasHttpManager m where
  getHttpManager = liftIO createRfcManager
