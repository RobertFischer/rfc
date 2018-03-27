{-# LANGUAGE ForeignFunctionInterface #-}

module RFC.GHCJS.Console
	( module RFC.GHCJS.Console
	) where

import           RFC.Miso.String
import           RFC.Prelude

foreign import javascript safe
	"console.info($1);"
	logInfo :: MisoString -> IO ()

foreign import javascript safe
	"console.warn($1);"
	logWarn :: MisoString -> IO ()

foreign import javascript safe
	"console.error($1);"
	logError :: MisoString -> IO ()

