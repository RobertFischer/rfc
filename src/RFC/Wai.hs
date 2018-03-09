{-# LANGUAGE NoImplicitPrelude #-}

module RFC.Wai
  ( runApplication
  , defaultMiddleware
  , module Network.Wai
  ) where

import           Control.Logger.Simple
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Network.Wai
import           Network.Wai.Cli
import           Network.Wai.Handler.Warp                  (Settings,
                                                            defaultSettings,
                                                            runSettings)
import           Network.Wai.Middleware.AcceptOverride
import           Network.Wai.Middleware.Approot            (envFallback)
import           Network.Wai.Middleware.Autohead
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.Gzip
import           Network.Wai.Middleware.Jsonp
import           Network.Wai.Middleware.MethodOverridePost
import           Network.Wai.Middleware.RequestLogger      (logStdout,
                                                            logStdoutDev)
import           RFC.Env                                   (isDevelopment)
import           RFC.Prelude
import           System.IO.Temp                            (createTempDirectory, getCanonicalTemporaryDirectory)

runApplication :: Application -> IO ()
runApplication app = do
  showEnv
  middlewares <- defaultMiddleware
  warpSettings <- decodeEnv
  reuseSocket <- bindSocketReusePort $ getPort warpSettings
  runGraceful
    ServeNormally
    (flip runSettingsSocket $ reuseSocket)
    warpSettings
    app
{-# INLINE runApplication #-}

bindSocketReusePort :: PortNumber -> IO Socket
bindSocketReusePort p =
  bracketOnError (socket AF_INET Stream defaultProtocol) close $ \sock -> do
    mapM_ (uncurry $ setSocketOption sock)
          [ (NoDelay  , 1)
          , (ReuseAddr, 1)
          , (ReusePort, 1) -- <-- Here we add the SO_REUSEPORT flag.
          ]
    bind sock $ SockAddrInet p $ tupleToHostAddress (127, 0, 0, 1)
    listen sock (max 2048 maxListenQueue)
    return sock

instance FromEnv Settings where
  fromEnv = snd $ (flip runStateT) defaultSettings $ do
    portNumber <- lift $ envWithDefault 3000
    modify $ setPort portNumber
    modify $ setOnExceptionResponse $
      if isDevelopment then
        exceptionResponseForDebug
      else
        -- TODO Render a JSON error response
        defaultOnExceptionResponse
    modify $ setServerName mempty
    return ()


defaultMiddleware :: IO Middleware
defaultMiddleware = do
  approot <- envFallback
  tmpDir <- getTmpDir
  gzipDir <- mkGzipDir tmpDir
  return $
    methodOverridePost .
    acceptOverride .
    (cors $ const $ Just corsConfig).
    autohead .
    jsonp .
    approot .
    gzip (gzipConfig gzipDir) .
    (if isDev then logStdoutDev else logStdout)
  where
    isDev = isDevelopment
    handleError msg e = do
      logError . cs $ "Error while: " ++ msg
      logError . cs . show $ e
      throwIO e
    getTmpDir = catchAnyDeep getCanonicalTemporaryDirectory (handleError "getting temp dir")
    mkGzipDir tmpDir = catchAnyDeep (createTempDirectory tmpDir "wai-gzip-middleware") (handleError "making temp dir")
    corsConfig = simpleCorsResourcePolicy
      { corsRequireOrigin = False
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
