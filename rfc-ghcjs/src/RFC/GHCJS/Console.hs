{-# LANGUAGE ForeignFunctionInterface #-}

module RFC.GHCJS.Console
	( module RFC.GHCJS.Console
	) where

import           Data.JSString
import           RFC.Prelude

foreign import javascript safe
	"console.info($1);"
	logInfo :: JSString -> IO ()

foreign import javascript safe
	"console.warn($1);"
	logWarn :: JSString -> IO ()

foreign import javascript safe
	"console.error($1);"
	logError :: JSString -> IO ()

