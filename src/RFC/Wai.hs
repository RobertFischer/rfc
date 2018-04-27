{-# LANGUAGE CPP              #-}
{-# LANGUAGE RebindableSyntax #-}

module RFC.Wai
  ( runApplication
  , defaultMiddleware
  , module Network.Wai
  ) where

import           Control.Logger.Simple                     hiding ( (<>) )
import           Control.Monad.State.Lazy                  hiding ( fail, mapM_ )
import           Network
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Network.Socket
import           Network.Wai
import           Network.Wai.Cli                           hiding ( socket )
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.Warp.Internal
import           Network.Wai.Middleware.AcceptOverride
import           Network.Wai.Middleware.Approot            ( envFallback )
import           Network.Wai.Middleware.Autohead
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.Gzip
import           Network.Wai.Middleware.Jsonp
import           Network.Wai.Middleware.MethodOverridePost
import           Network.Wai.Middleware.RequestLogger      ( logStdout, logStdoutDev )
import           RFC.Env
  ( FromEnv (..), decodeEnv, envMaybe, isDevelopment, showEnv, (.!=) )
import           RFC.Log
import           RFC.Prelude
import           System.IO.Temp
  ( createTempDirectory, getCanonicalTemporaryDirectory )

logStartup :: String -> IO ()
logStartup = logInfo . cs

runApplication :: Int -> Application -> IO ()
runApplication port app = withLogging . withSocketsDo $ do
  logStartup $
    "Initializing environment. Is this a development environment? " <>
    (if isDevelopment then "yes" else "NO!")
  showEnv
  logStartup "Inferring Warp settings from the environment"
  rawWarpSettingsResult <- decodeEnv :: IO (Either String Settings)
  case rawWarpSettingsResult of
    Left err -> fail $ "Error while inferring Warp settings: " <> err
    Right origWarpSettings -> do
      logStartup "Instantiating middleware"
      middlewares <- defaultMiddleware
      let warpSettings = origWarpSettings { settingsPort = port }
      logStartup "Binding socket to be SO_REUSEPORT"
      reuseSocket <- bindSocketReusePort port
      logStartup "Running the app gracefully"
      runGraceful
        ServeNormally
        (`runSettingsSocket` reuseSocket)
        warpSettings
        (middlewares app)
{-# INLINE runApplication #-}

bindSocketReusePort :: Port -> IO Socket
bindSocketReusePort p =
  bracketOnError (socket AF_INET Stream defaultProtocol) close $ \sock -> do
    mapM_ (uncurry $ setSocketOption sock) $ filter (isSupportedSocketOption . fst)
          [ (NoDelay  , 1)
          , (KeepAlive, 1)
#ifndef DEVELOPMENT
          , (ReuseAddr, 1)
          , (ReusePort, 1) -- <-- Here we add the SO_REUSEPORT flag.
#endif
          ]
    bind sock $ SockAddrInet (fromIntegral p) iNADDR_ANY
    listen sock (min 2048 maxListenQueue)
    return sock

instance FromEnv Settings where
  fromEnv = do
    portNumber <- envMaybe "PORT" .!= 3000
    return
      . setPort portNumber
      . setOnExceptionResponse
        (if isDevelopment then
          exceptionResponseForDebug
        else
          -- TODO Render a JSON error response
          defaultOnExceptionResponse
        )
      $ setServerName emptyUTF8 defaultSettings


defaultMiddleware :: IO Middleware
defaultMiddleware = do
  logStartup "Inferring the application root"
  approot <- envFallback
  logStartup "Looking up temporary directory"
  tmpDir <- getTmpDir
  logStartup $ "Creating a temporary directory for gzip caching under " <> tmpDir
  gzipDir <- mkGzipDir tmpDir
  logStartup $ "Created gzip caching directory: " <> gzipDir
  return $
    methodOverridePost .
    acceptOverride .
    (cors . const $ Just corsConfig).
    autohead .
    jsonp .
    approot .
    gzip (gzipConfig gzipDir) .
    (if isDev then logStdoutDev else logStdout)
  where
    isDev = isDevelopment
    handleError msg e = do
      logError . cs $ "Error while: " <> msg
      logError . cs . show $ e
      throwIO e
    getTmpDir = catchAnyDeep getCanonicalTemporaryDirectory (handleError "getting temp dir")
    mkGzipDir tmpDir = catchAnyDeep (createTempDirectory tmpDir "wai-gzip-middleware") (handleError "making temp dir")
    corsConfig = simpleCorsResourcePolicy
      { corsRequireOrigin = False
      , corsOrigins = Nothing
      , corsVaryOrigin = False
      , corsMaxAge = Nothing
      , corsRequestHeaders = hContentType : simpleHeaders
      , corsMethods =
        [ methodGet
        , methodPost
        , methodHead
        , methodDelete
        , methodPatch
        , methodOptions
        , methodPut
        ]
      }
    gzipConfig gzipDir = def
      { gzipFiles = GzipCacheFolder gzipDir
      , gzipCheckMime = const True
      }
{-# INLINABLE defaultMiddleware #-}
