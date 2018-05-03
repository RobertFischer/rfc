{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}

module RFC.GHCJS.Navigation
  ( getCurrentURI
  ) where

import           Network.URI
import           RFC.GHCJS.Console ( logInfo )
import           RFC.Miso.String
import           RFC.Prelude

getCurrentURI :: (MonadIO m) => m URI
getCurrentURI = liftIO $ do
	uriStr <- fromMisoString <$> getCurrentURIMisoString
	case parseURI uriStr of
		Nothing -> fail $ "Could not parse the current URI: " <> uriStr
		Just uri -> do
			logInfo . toMisoString $ "Retrieved current URI. Fragment: " <> (uriFragment uri)
			return uri

foreign import javascript safe
  "window.location.href"
  getCurrentURIMisoString :: IO MisoString
