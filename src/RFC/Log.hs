module RFC.Log
  ( module Control.Logger.Simple
  , module RFC.Log
  ) where

import RFC.Prelude
import RFC.Env as Env
import Control.Logger.Simple (pureDebug, pureInfo, pureWarn, pureError, logDebug, logInfo, logWarn, logError)
import Control.Logger.Simple as Log
import System.IO (stderr, hSetBuffering, BufferMode(..))

withLogging :: IO a -> IO a
withLogging action = do
  isDev <- Env.isDevelopment
  hSetBuffering stderr LineBuffering
  Log.withGlobalLogging (logConfig isDev) action
  where
    logConfig isDev =
      if isDev then
          Log.LogConfig { Log.lc_file = Nothing, Log.lc_stderr = True }
      else
          Log.LogConfig { Log.lc_file = Just "./log/api-server.log", Log.lc_stderr = False }


