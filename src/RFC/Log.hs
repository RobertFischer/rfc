module RFC.Log
  ( module Control.Logger.Simple
  , module RFC.Log
  ) where

import RFC.Prelude
import Control.Logger.Simple (pureDebug, pureInfo, pureWarn, pureError, logDebug, logInfo, logWarn, logError)
import Control.Logger.Simple as Log
import System.IO (stderr, hSetBuffering, BufferMode(..))

withLogging :: IO a -> IO a
withLogging action = do
  hSetBuffering stderr LineBuffering
  Log.withGlobalLogging logConfig action
  where
    logConfig = Log.LogConfig { Log.lc_file = Nothing, Log.lc_stderr = True }

