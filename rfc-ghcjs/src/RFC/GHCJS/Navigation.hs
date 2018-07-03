{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}

module RFC.GHCJS.Navigation
  ( getCurrentURI
  ) where

import           Data.JSString
import           Network.URI
import           RFC.GHCJS.Console ( logInfo )
import           RFC.Prelude

getCurrentURI :: (MonadIO m) => m URI
getCurrentURI = liftIO $ do
	uriStr <- unpack <$> getCurrentURIJSString
	case parseURI uriStr of
		Nothing -> fail $ "Could not parse the current URI: " <> uriStr
		Just uri -> do
			logInfo . pack $ "Retrieved current URI. Fragment: " <> (uriFragment uri)
			return uri

foreign import javascript safe
  "window.location.href"
  getCurrentURIJSString :: IO JSString
