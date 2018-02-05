{-# LANGUAGE NoImplicitPrelude #-}

module RFC.Log
  ( module Control.Logger.Simple
  , module RFC.Log
  ) where

import           Control.Logger.Simple (logDebug, logError, logInfo, logWarn,
                                        pureDebug, pureError, pureInfo,
                                        pureWarn)
import           Control.Logger.Simple as Log
import           RFC.Env               as Env
import           RFC.Prelude
import           System.IO             (BufferMode (..), stderr)

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


