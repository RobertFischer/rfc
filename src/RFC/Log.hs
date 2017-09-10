module RFC.Log
  ( module Control.Logger.Simple
  , module RFC.Log
  ) where

import RFC.Prelude
import Control.Logger.Simple (pureDebug, pureInfo, pureWarn, pureError, logDebug, logInfo, logWarn, logError)
import Control.Logger.Simple as Log

withLogging :: IO a -> IO a
withLogging = Log.withGlobalLogging Log.LogConfig { Log.lc_file = Nothing, Log.lc_stderr = True }

