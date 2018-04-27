{-# LANGUAGE NoImplicitPrelude #-}

module RFC.Log
  ( module Control.Logger.Simple
  , withLogging
  ) where

import           Control.Logger.Simple (logDebug, logError, logInfo, logWarn,
                                        pureDebug, pureError, pureInfo,
                                        pureWarn)
import qualified Control.Logger.Simple as Log
import           Data.Time.Format
import           Data.Time.LocalTime
import           RFC.Env               as Env
import           RFC.Prelude
import           System.IO             (BufferMode (..), stderr)

withLogging :: (MonadUnliftIO m, MonadFail m) => m a -> m a
withLogging action = do
  hSetBuffering stderr LineBuffering
  ioAction <- toIO action
  appSlug <- Env.readAppSlug
  timestamp <- formatTime defaultTimeLocale "%_Y%m%d.%H%M.%S%Q.%Z" <$> liftIO getZonedTime
  liftIO $ Log.withGlobalLogging (logConfig appSlug timestamp) ioAction
  where
    logConfig appSlug timestamp =
      if Env.isDevelopment then
          Log.LogConfig { Log.lc_file = Nothing, Log.lc_stderr = True }
      else
          Log.LogConfig { Log.lc_file = Just ("./log/" <> appSlug <> "." <> timestamp <> ".log"), Log.lc_stderr = False }


