{-# LANGUAGE CPP #-}

module RFC.Env
  ( isDevelopment
  , readEnvironment
  , readHost
  , readPort
  ) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.Either
import           Data.Word
import           Database.PostgreSQL.Simple as Psql
import           Database.Redis             as Redis
import           GHC.TypeLits               (Symbol)
import           RFC.Prelude
import           System.Environment

-- TODO Create a Monad that only logs reading the env var once, and reads all the environment variables at once, and is pure.

isDevelopment :: (MonadIO m) => m Bool
isDevelopment = fmap ((==) "development") readEnvironment

forDevOnly :: (MonadIO m) => a -> m (Maybe a)
#ifdef DEVELOPMENT
forDevOnly defaultValue = Just defaultValue
#else
forDevOnly defaultValue = do
  isDev <- isDevelopment
  return $ isDev <|> Just defaultValue
#endif

readEnvironment :: (MonadIO m) => m String
readEnvironment = readEnv "ENV" "development"

readAppSlug :: (MonadIO m) => m String
readAppSlug = readEnv "APP_SLUG" Nothing

readHost :: (MonadIO m) => m String
readHost =
  forDevOnly "localhost" >>= readEnv "HOST"

readPort :: (MonadIO m) => Word16 -> m Word16
readPort devPort = do
  defaultPort <- forDevOnly $ show devPort
  result <- readEnv "PORT" defaultPort
  return $ (read result :: Word16)

readEnv name defaultValue = liftIO $ do
  result <- lookupEnv name
  return $ result <|> defaultValue
