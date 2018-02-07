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
  , module Network.Wreq.Session
  , module Network.URL
  , module Network.HTTP.Types.Status
  ) where

import           Control.Lens
import           Network.HTTP.Client       (Manager, ManagerSettings,
                                            newManager)
import           Network.HTTP.Client.TLS   (tlsManagerSettings)
import           Network.HTTP.Types.Status hiding (statusCode, statusMessage)
import           Network.URL
import           Network.Wreq.Lens
import           Network.Wreq.Session      hiding (withAPISession)
import           RFC.JSON                  (FromJSON, decodeOrDie)
import           RFC.Prelude
import           RFC.String

rfcManagerSettings :: ManagerSettings
rfcManagerSettings = tlsManagerSettings

createRfcManager :: IO Manager
createRfcManager = newManager rfcManagerSettings

withAPISession :: (Session -> IO a) -> IO a
withAPISession = (>>=) $ newSessionControl Nothing rfcManagerSettings

newtype BadStatusException = BadStatusException (Status,URL)
  deriving (Show,Eq,Ord,Generic,Typeable)
instance Exception BadStatusException

apiExecute :: (MonadThrow m, HasAPIClient m, MonadIO m, ConvertibleString LazyByteString s)  =>
  URL -> (Session -> String -> IO (Response LazyByteString)) -> (s -> m a) -> m a
apiExecute rawUrl action converter = do
    session <- getAPIClient
    response <- liftIO $ action session url
    let status = response ^. responseStatus
    case status ^. statusCode of
      200 -> converter . cs $ response ^. responseBody
      _   -> throwM $ badResponseStatus status
  where
    url = exportURL rawUrl
    badResponseStatus status = BadStatusException (status, rawUrl)

apiGet :: (HasAPIClient m, FromJSON a, MonadIO m, MonadCatch m, Exception e) => URL -> (e -> m a) -> m a
apiGet url onError =
    handle onError $ apiExecute url get decodeOrDie

class HasAPIClient m where
  getAPIClient :: m Session

class HasHttpManager m where
  getHttpManager :: m Manager

instance (MonadIO m) => HasHttpManager m where
  getHttpManager = liftIO createRfcManager
