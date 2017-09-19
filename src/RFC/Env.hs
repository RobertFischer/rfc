module RFC.Env
  ( isDevelopment
  , readEnvironment
  , readGoogleMapsAPIKey
  ) where

import RFC.Prelude
import Control.Logger.Simple (logInfo)
import System.Environment (lookupEnv)

-- TODO Create a Monad that only logs reading the env var once, and reads all the environment variables at once, and is pure.

readEnv :: String -> Maybe String -> IO String
readEnv envKey defaultValue = do
  foundValue <- lookupEnv envKey
  case foundValue of
    Nothing ->
      case defaultValue of
        Nothing -> fail $ "No value of " ++ (show envKey) ++ " environment variable, and no default configured."
        (Just s) -> return s
    (Just s) -> do
      logInfo (cs $ "Reading Env Var. Key=" ++ (show envKey) ++ " Value=" ++ (show s))
      return s

isDevelopment :: IO Bool
isDevelopment = ((==) "development") <$> readEnvironment

readEnvironment :: IO String
readEnvironment = readEnv "ENV" defaultEnv
  where
    defaultEnv = Just "development"

readGoogleMapsAPIKey :: IO String
readGoogleMapsAPIKey = readEnv "GMAPS_API_KEY" Nothing
