{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}

module RFC.HTTP.Client
  ( withAPISession
  , HasAPIClient(..)
  , HasHttpManager(..)
  , BadStatusException
  , apiGet
  , module Network.Wreq.Session
  , module Network.HTTP.Types.Status
  , geocodeGet
  , GoogleGeocodeResult(..)
  ) where

import           Control.Lens
import           Data.Aeson                as JSON
import           Data.Aeson.Types          as JSON
import qualified Data.Scientific           as Sci
import           Data.Vector               ( (!?) )
import           Network.HTTP.Client       ( Manager, ManagerSettings, newManager )
import           Network.HTTP.Client.TLS   ( tlsManagerSettings )
import           Network.HTTP.Types.Status hiding ( statusCode, statusMessage )
import           Network.URI               as URI
import           Network.Wreq.Lens
import           Network.Wreq.Session      hiding ( withAPISession )
import           RFC.Prelude
import           Servant.Server

data GoogleGeocodeResult = GoogleGeocodeResult
  { ggrLat  :: Float
  , ggrLng  :: Float
  , ggrAddr :: StrictText
  } deriving (Eq,Ord,Show,Generic,Typeable)
instance FromJSON GoogleGeocodeResult where
  parseJSON = JSON.withObject "GoogleGeocodeResult" $ \v -> do
      resultsAry <- v .: "results"
      result <- JSON.withArray "results[0]" extractHead resultsAry
      withObject "results[0] content" parseResult result
    where
      extractHead :: JSON.Array -> JSON.Parser JSON.Value
      extractHead vec =
        maybe
          (fail "Empty results array returned")
          return
          (vec !? 0)
      parseResult :: JSON.Object -> JSON.Parser GoogleGeocodeResult
      parseResult obj = do
        addr <- obj .: "formatted_address" >>= withText "results[0].formatted_address" return
        geometry <- obj .: "geometry" >>= withObject "results[0].geometry" return
        location <- geometry .: "location" >>= withObject "results[0].geometry.location" return
        lat <- location .: "lat" >>= withScientific "results[0].geometry.location.lat" (return . Sci.toRealFloat)
        lng <- location .: "lng" >>= withScientific "results[0].geometry.location.lng" (return . Sci.toRealFloat)
        return GoogleGeocodeResult { ggrLat = lat, ggrLng = lng, ggrAddr = addr }

rfcManagerSettings :: ManagerSettings
rfcManagerSettings = tlsManagerSettings

createRfcManager :: (MonadIO m) => m Manager
createRfcManager = liftIO $ newManager rfcManagerSettings

withAPISession :: (MonadIO m) => (Session -> m a) -> m a
withAPISession = (>>=) $ liftIO (newSessionControl Nothing rfcManagerSettings)

newtype BadStatusException = BadStatusException (Status,URI)
  deriving (Show,Eq,Ord,Generic,Typeable)
instance Exception BadStatusException

apiExecute :: (HasAPIClient m, MonadUnliftIO m)  =>
  URI -> (Session -> String -> IO (Response LazyByteString)) -> (LazyByteString -> m a) -> m a
apiExecute rawUrl action converter = webExecute rawUrl action >>= converter

webExecute :: (HasAPIClient m, MonadUnliftIO m) =>
  URI -> (Session -> String -> IO (Response LazyByteString)) -> m LazyByteString
webExecute rawUrl action = do
  session <- getAPIClient
  response <- liftIO $ action session url
  let status = response ^. responseStatus
  case status ^. statusCode of
    200 -> return $ response ^. responseBody
    _   -> throwIO $ badResponseStatus status
  where
    url = show rawUrl
    badResponseStatus status = BadStatusException (status, rawUrl)

apiGet :: (HasAPIClient m, FromJSON a, MonadUnliftIO m, Exception e) => URI -> (e -> m a) -> m a
apiGet url onError =
  handle onError $ apiExecute url get decodeOrDie

geocodeGet ::
  (HasAPIClient m, MonadUnliftIO m, Exception e) =>
  StrictText -> (e -> m GoogleGeocodeResult) -> m GoogleGeocodeResult
geocodeGet addr onError = handle onError $
    case URI.parseURI "https://maps.googleapis.com/maps/api/geocode/json" of
      Nothing -> throwIO err500 { errBody = "Cannot parse the geocoding URI" }
      Just uri -> do
        let address = URI.escapeURIString URI.isUnescapedInURIComponent (fromText addr)
        let apiKey = "AIzaSyD_pe9DkE2GwqMLcbHTfDta0He_OSb6qDo"
        let prefix = case uriQuery uri of
              "" -> "?"
              qry@('?':_) -> qry
              qry -> ('?':qry) <> "&"
        let uriWithQuery = uri { uriQuery = prefix <> "address=" <> address <> "&" <> "key=" <> apiKey }
        apiGet uriWithQuery onError

class HasAPIClient m where
  getAPIClient :: m Session

class HasHttpManager m where
  getHttpManager :: m Manager

instance (MonadIO m) => HasHttpManager m where
  getHttpManager = liftIO createRfcManager
