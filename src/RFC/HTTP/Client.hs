{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module RFC.HTTP.Client
  ( withAPISession
  , HasAPIClient(..)
  , BadStatusException
  , apiGet
  , module Network.Wreq.Session
  , module Network.URL
  , module Network.HTTP.Types.Status
  ) where

import           Control.Lens
import           Network.HTTP.Client.TLS   (tlsManagerSettings)
import           Network.HTTP.Types.Status hiding (statusCode, statusMessage)
import           Network.URL
import           Network.Wreq.Lens
import           Network.Wreq.Session      hiding (withAPISession)
import           RFC.JSON                  (FromJSON, decodeOrDie)
import           RFC.Prelude
import           RFC.String

withAPISession :: (Session -> IO a) -> IO a
withAPISession = (>>=) $ newSessionControl Nothing tlsManagerSettings

newtype BadStatusException = BadStatusException (Status,URL)
  deriving (Show,Eq,Ord,Generic,Typeable)
instance Exception BadStatusException

apiExecute :: (HasAPIClient m, ConvertibleString LazyByteString s)  =>
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

apiGet :: (HasAPIClient m, FromJSON a, MonadCatch m, Exception e) => URL -> (e -> m a) -> m a
apiGet url onError =
    handle onError $ apiExecute url get decodeOrDie

class (MonadThrow m, MonadIO m) => HasAPIClient m where
  getAPIClient :: m Session
