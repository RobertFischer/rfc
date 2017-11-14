module RFC.Env
  ( readGoogleMapsAPIKey
  , isDevelopment
  , readEnvironment
  , readPsqlConnectInfo
  , readRedisConnectInfo
  ) where

import RFC.Prelude
import System.Environment (lookupEnv)
import Database.PostgreSQL.Simple as Psql
import Database.Redis as Redis
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
          return s
    (Just s) -> do
      return s

isDevelopment :: (MonadIO m) =>  m Bool
isDevelopment = ((==) "development") <$> readEnvironment

forDevOnly :: (MonadIO m) => String -> m (Maybe String)
forDevOnly defaultValue = do
  isDev <- isDevelopment
  return $ if isDev then
    Just defaultValue
  else
    Nothing

readEnvironment :: (MonadIO m) => m String
readEnvironment = readEnv "ENV" defaultEnv
  where
    defaultEnv = Just "development"

readGoogleMapsAPIKey :: (MonadIO m) => m String
readGoogleMapsAPIKey = readEnv "GMAPS_API_KEY" Nothing

readPsqlUser :: (MonadIO m) => m String
readPsqlUser = do
  projectThunk <- readAppSlug >>= forDevOnly
  readEnv "PSQL_USERNAME" $ projectThunk

readPsqlPassword :: (MonadIO m) => m String
readPsqlPassword = do
  projectThunk <- readAppSlug >>= forDevOnly
  readEnv "PSQL_PASSWORD" projectThunk

readPsqlDatabase :: (MonadIO m) => m String
readPsqlDatabase = do
  projectThunk <- readAppSlug >>= forDevOnly
  readEnv "PSQL_DATABASE" projectThunk

readPsqlHost :: (MonadIO m) => m String
readPsqlHost = readEnv "PSQL_HOST" $ Just $ Psql.connectHost Psql.defaultConnectInfo

readPsqlPort :: (MonadIO m) => m Word16
readPsqlPort = do
  result <- readEnv "PSQL_PORT" $ Just $ show $ Psql.connectPort Psql.defaultConnectInfo
  return $ read result

readPsqlConnectInfo :: (MonadIO m) => m Psql.ConnectInfo
readPsqlConnectInfo =
  Psql.ConnectInfo
    <$> readPsqlHost
    <*> readPsqlPort
    <*> readPsqlUser
    <*> readPsqlPassword
    <*> readPsqlDatabase

readRedisConnectInfo :: (MonadIO m) => m Redis.ConnectInfo
readRedisConnectInfo =
  Redis.ConnInfo
    <$> readRedisHost
    <*> readRedisPort
    <*> readRedisPassword
    <*> readRedisDbNumber
    <*> (return $ Redis.connectMaxConnections Redis.defaultConnectInfo)
    <*> (return $ Redis.connectMaxIdleTime Redis.defaultConnectInfo)
    <*> (return $ Just 10)

readRedisHost :: (MonadIO m) => m Redis.HostName
readRedisHost = readEnv "REDIS_HOST" $ Just $ Redis.connectHost Redis.defaultConnectInfo

readRedisPort :: (MonadIO m) => m Redis.PortID
readRedisPort = do
  result <- readEnv "REDIS_PORT" $ Just "6379" -- Default Redis port
  return $ PortNumber $ read result

readRedisPassword :: (MonadIO m) => m (Maybe ByteString)
readRedisPassword = do
  result <- readEnv "REDIS_PASSWORD" $ Just ""
  return $
    case result of
      "" -> Nothing
      _ -> Just $ cs result

readRedisDbNumber :: (MonadIO m) => m Integer
readRedisDbNumber = read <$> readEnv "REDIS_DATABASE" (Just $ show $ Redis.connectDatabase Redis.defaultConnectInfo)

readAppSlug :: (MonadIO m) => m String
readAppSlug = readEnv "APP_SLUG" Nothing
