module RFC.Env
  ( module RFC.Env
  ) where

import RFC.Prelude
import Control.Logger.Simple (logInfo, logWarn)
import System.Environment (lookupEnv)
import Database.PostgreSQL.Simple as Psql
import Data.Word (Word16)

-- TODO Create a Monad that only logs reading the env var once, and reads all the environment variables at once, and is pure.

readEnv :: (MonadIO m) => String -> Maybe String -> m String
readEnv envKey defaultValue = do
  foundValue <- liftIO $ lookupEnv envKey
  case foundValue of
    Nothing ->
      case defaultValue of
        Nothing -> fail $ "No value of " ++ (show envKey) ++ " environment variable, and no default configured."
        (Just s) -> do
          logWarn . cs $ "No value of " ++ (show envKey) ++ " environment variable, using default " ++ (show s)
          return s
    (Just s) -> do
      logInfo . cs $ "Reading Env Var. Key=" ++ (show envKey) ++ " Value=" ++ (show s)
      return s

isDevelopment :: (MonadIO m) =>  m Bool
isDevelopment = ((==) "development") <$> readEnvironment

readEnvironment :: (MonadIO m) => m String
readEnvironment = readEnv "ENV" defaultEnv
  where
    defaultEnv = Just "development"

readGoogleMapsAPIKey :: (MonadIO m) => m String
readGoogleMapsAPIKey = readEnv "GMAPS_API_KEY" Nothing

readPsqlUser :: (MonadIO m) => String -> m String
readPsqlUser projectThunk = readEnv "PSQL_USERNAME" $ Just projectThunk

readPsqlPassword :: (MonadIO m) => String -> m String
readPsqlPassword projectThunk = readEnv "PSQL_PASSWORD" $ Just projectThunk

readPsqlDatabase :: (MonadIO m) => String -> m String
readPsqlDatabase projectThunk = readEnv "PSQL_DATABASE" $ Just projectThunk

readPsqlHost :: (MonadIO m) => m String
readPsqlHost = readEnv "PSQL_HOST" $ Just $ Psql.connectHost Psql.defaultConnectInfo

readPsqlPort :: (MonadIO m) => m Word16
readPsqlPort = do
  result <- readEnv "PSQL_PORT" $ Just $ show $ Psql.connectPort Psql.defaultConnectInfo
  return $ read result

readConnectInfo :: (MonadIO m) => String -> m ConnectInfo
readConnectInfo projectThunk =
  ConnectInfo
    <$> readPsqlHost
    <*> readPsqlPort
    <*> readPsqlUser projectThunk
    <*> readPsqlPassword projectThunk
    <*> readPsqlDatabase projectThunk
