module RFC.Wai
  ( defaultMiddleware
  ) where

import RFC.Prelude
import RFC.Env (isDevelopment)
import Network.Wai
import Network.Wai.Middleware.AcceptOverride
import Network.Wai.Middleware.Approot (envFallback)
import Network.Wai.Middleware.Autohead
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.Jsonp
import Network.Wai.Middleware.MethodOverridePost
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import System.IO.Temp (getCanonicalTemporaryDirectory, createTempDirectory)
import Network.Wai.Middleware.Cors
import Network.HTTP.Types.Method
import Network.HTTP.Types.Header

defaultMiddleware :: IO Middleware
defaultMiddleware = do
  isDev <- isDevelopment
  approot <- envFallback
  tmpDir <- getCanonicalTemporaryDirectory
  gzipDir <- createTempDirectory tmpDir "wai-gzip-middleware"
  return $
    (cors $ const $ Just corsConfig).
    autohead .
    acceptOverride .
    jsonp .
    approot .
    methodOverridePost .
    gzip (gzipConfig gzipDir) .
    (if isDev then logStdoutDev else logStdout)
  where
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
        ]
      }
    gzipConfig gzipDir = def
      { gzipFiles = GzipCacheFolder gzipDir
      , gzipCheckMime = const True
      }
