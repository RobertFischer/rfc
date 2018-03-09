{-# LANGUAGE CPP          #-}
{-# LANGUAGE InstanceSigs #-}

module RFC.Env
  ( isDevelopment
  , readEnvironment
  , readHost
  , readPort
  , readAppSlug
  , forDevOnly
  , envWithDefault
  , envWithDevDefault
  , module System.Envy
  ) where

import           Control.Applicative
import           Data.Time.Clock
import           Data.Word
import           Network             (PortID (..))
import           RFC.Prelude
import           System.Environment
import           System.Envy
import           Text.Read           (readMaybe)

envWithDefault :: Var a => String -> a -> Parser a
envWithDefault name defaultValue = fmap (fromMaybe defaultValue) $ envMaybe name
{-# INLINE envWithDefault #-}

envWithDevDefault :: Var a => String -> a -> Parser a
envWithDevDefault name defaultValue =
  if isDevelopment then
    envWithDefault name defaultValue
  else
    env name
{-# INLINE envWithDevDefault #-}

isDevelopment :: Bool
isDevelopment =
#ifdef DEVELOPMENT
  True
#else
  False
#endif
{-# INLINE isDevelopment #-}

forDevOnly :: a -> Maybe a
forDevOnly defaultValue =
  if isDevelopment then
    Just defaultValue
  else
    Nothing
{-# INLINE forDevOnly #-}

readEnvironment :: (MonadIO m) => m String
readEnvironment =
  readEnvWithDefault "ENV" "development"
{-# INLINE readEnvironment #-}

readAppSlug :: (MonadIO m, MonadFail m) => m String
readAppSlug = readEnvWithDevDefault "APP_SLUG" "dev"
{-# INLINE readAppSlug #-}

readHost :: (MonadIO m, MonadFail m) => m String
readHost =
  readEnvWithDevDefault "HOST" "localhost"
{-# INLINE readHost #-}

readPort :: (MonadIO m, MonadFail m) => Word16 -> m Word16
readPort devPort = readEnvWithDevDefault "PORT" devPort
{-# INLINE readPort #-}

readEnvWithDefault :: (MonadIO m, Read a) => String -> a -> m a
readEnvWithDefault name defaultValue =
  either (const defaultValue) id <$> readEnv name
{-# INLINE readEnvWithDefault #-}

readEnvWithDevDefault :: (MonadIO m, MonadFail m, Read a) => String -> a -> m a
readEnvWithDevDefault =
  if isDevelopment then
    readEnvWithDefault
  else
    (\name _ -> readEnvOrDie name)
{-# INLINE readEnvWithDevDefault #-}

readEnv :: (MonadIO m, Read a) => String -> m (Either String a)
readEnv name = liftIO $ do
  result <- lookupEnv name
  return $ case result >>= readMaybe of
    Nothing        -> Left (show result)
    Just goodValue -> Right goodValue
{-# INLINE readEnv #-}

readEnvOrDie :: (MonadIO m, MonadFail m, Read a) => String -> m a
readEnvOrDie name = do
  maybeResult <- readEnv name
  case maybeResult of
    Left err ->
      if err == (show (Nothing::Maybe String)) then
        fail $ "No value set for mandatory environment variable: " ++ name
      else
        fail $ "Cannot use value set for mandatory environment variable: " ++ name  ++ " => " ++ err
    Right result -> return result
{-# INLINE readEnvOrDie #-}

instance Var NominalDiffTime where
  toVar :: NominalDiffTime -> String
  toVar = show
  {-# INLINE toVar #-}

  fromVar :: String -> Maybe NominalDiffTime
  fromVar var = fromInteger <$> readMaybe var
  {-# INLINE fromVar #-}

instance Var PortID where
  toVar (PortNumber portNum) = toVar $ toInteger portNum
  toVar _                    = error "Can only write port numbers to var"
  {-# INLINE toVar #-}

  fromVar = (fmap $ PortNumber . fromInteger) . fromVar
  {-# INLINE fromVar #-}
