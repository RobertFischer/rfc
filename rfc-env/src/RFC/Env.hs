{-# LANGUAGE CPP                 #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RFC.Env
  ( isDevelopment
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
import           Network             ( PortID (..) )
import           RFC.Prelude
import           System.Environment
import           System.Envy
import           Text.Read           ( readEither, readMaybe )

envWithDefault :: Var a => String -> a -> Parser a
envWithDefault name defaultValue = fromMaybe defaultValue <$> envMaybe name
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

readAppSlug :: (MonadIO m, MonadFail m) => m String
readAppSlug = readEnvStringWithDevDefault "APP_SLUG" "dev"
{-# INLINE readAppSlug #-}

readHost :: (MonadIO m, MonadFail m) => m String
readHost =
  readEnvStringWithDevDefault "HOST" "localhost"
{-# INLINE readHost #-}

readPort :: (MonadIO m, MonadFail m) => Word16 -> m Word16
readPort = readEnvWithDevDefault "PORT"
{-# INLINE readPort #-}

readEnvStringWithDefault :: (MonadIO m) => String -> String -> m String
readEnvStringWithDefault name defaultValue =
  fromMaybe defaultValue <$> readEnvString name
{-# INLINE readEnvStringWithDefault #-}

readEnvStringWithDevDefault :: (MonadIO m, MonadFail m) => String -> String -> m String
readEnvStringWithDevDefault =
  if isDevelopment then
    readEnvStringWithDefault
  else
    (\name _ -> do -- Need to join the MonadFail instances
      result <- readEnvString name
      case result of
        Left err -> fail err
        Right x  -> return x
    )
{-# INLINE readEnvStringWithDevDefault #-}

readEnvWithDefault :: (MonadIO m, Read a) => String -> a -> m a
readEnvWithDefault name defaultValue = do
  maybeVal <- readEnvString name
  return . fromMaybe defaultValue $ maybeVal >>= readMaybe
{-# INLINE readEnvWithDefault #-}

readEnvWithDevDefault :: (MonadIO m, MonadFail m, Read a) => String -> a -> m a
readEnvWithDevDefault =
  if isDevelopment then
    readEnvWithDefault
  else
    (\name _ -> do
      errOrResult <- readEnv name
      case errOrResult of
        Left err -> fail err
        Right x  -> return x
    )
{-# INLINE readEnvWithDevDefault #-}

readEnvString :: (MonadIO m1, MonadFail m2) => String -> m1 (m2 String)
readEnvString name = do
  result <- liftIO $ lookupEnv name
  return $ case result of
    Nothing -> fail $ "No value set for environment variable: " <> name
    Just x  -> return x
{-# INLINE readEnvString #-}

readEnv :: (MonadIO m1, MonadFail m2, Read a) => String -> m1 (m2 a)
readEnv name = do
  eitherOrVal <- readEnvString name
  return $ case eitherOrVal :: ((Either String) String) of
    Left err -> fail err
    Right val ->
      case readEither val of
        Left err -> fail $ "Error parsing value from environment variable. " <> name <> " = " <> (show val) <> " => " <> err
        Right result -> return result
{-# INLINE readEnv #-}

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

  fromVar = fmap (PortNumber . fromInteger) . fromVar
  {-# INLINE fromVar #-}
