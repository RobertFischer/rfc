{-# LANGUAGE NoImplicitPrelude #-}

module RFC.Log
  ( module Control.Logger.Simple
  , withLogging
  ) where

import           Control.Logger.Simple (logDebug, logError, logInfo, logWarn,
                                        pureDebug, pureError, pureInfo,
                                        pureWarn)
import           Control.Logger.Simple as Log
import           RFC.Env               as Env
import           RFC.Prelude
import           System.IO             (BufferMode (..), stderr)

withLogging :: (MonadUnliftIO m) => m a -> m a
withLogging action = do
  isDev <- Env.isDevelopment
  hSetBuffering stderr LineBuffering
  ioAction <- toIO action
  liftIO $ Log.withGlobalLogging (logConfig isDev) ioAction
  where
    logConfig isDev =
      if isDev then
          Log.LogConfig { Log.lc_file = Nothing, Log.lc_stderr = True }
      else
          Log.LogConfig { Log.lc_file = Just "./log/server.log", Log.lc_stderr = False }


