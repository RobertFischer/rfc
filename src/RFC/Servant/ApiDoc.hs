{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RecordWildCards       #-}

module RFC.Servant.ApiDoc
  ( apiToHtml
  , apiToAscii
  , apiToSwagger
  , apiApplication
  ) where

import           Data.Aeson.Types                (fromEncoding, toEncoding)
import qualified Data.Binary.Builder             as Builder
import           Data.Char                       as Char
import           Data.Default                    (def)
import           Network.HTTP.Types.Header       (hContentType)
import           Network.HTTP.Types.Status
import           Network.Wai
import           RFC.Prelude
import           RFC.Servant
import           RFC.String
import           Servant.Swagger
import qualified Text.Blaze.Html.Renderer.String as Blaze
import qualified Text.Markdown                   as MD

apiToHtml :: (HasDocs a) => Proxy a -> Html
apiToHtml = preEscapedToHtml . (MD.markdown mdSettings) . cs . markdown . docs
  where
    mdSettings = def
      { MD.msLinkNewTab = False
      , MD.msAddHeadingId = True
      }

apiToAscii :: (ConvertibleString String s, HasDocs a) => Proxy a -> s
apiToAscii = cs . markdown . docs

apiToSwagger :: (HasSwagger a) => Proxy a -> Swagger
apiToSwagger = toSwagger

apiApplication :: (HasDocs a, HasSwagger a) => Proxy a -> Application
apiApplication api request callback =
  case reqMethod of
    "GET" -> checkPath
    _     -> failMethodNotAllowed
  where
    html = Blaze.renderHtml $ apiToHtml api
    ascii = apiToAscii api
    swaggerToLbs = Builder.toLazyByteString . fromEncoding . toEncoding
    swagger = swaggerToLbs $ apiToSwagger api
    reqMethod = map Char.toUpper $ cs $ requestMethod request
    pathInfo = map Char.toLower $ cs $ rawPathInfo request
    checkPath =
      case map Char.toLower (cs $ rawPathInfo request) of
        "swagger.json"  -> serveSwagger
        "/swagger.json" -> serveSwagger
        "api.html"      -> serveHtml
        "/api.html"     -> serveHtml
        "api.txt"       -> serveTxt
        "/api.txt"      -> serveTxt
        _               -> failPathNotFound
    response contentType body = callback $
      responseLBS status200 [(hContentType, cs contentType)] body
    serveHtml = response "text/html" (cs html)
    serveTxt = response "text/plain" ascii
    serveSwagger = response "application/json" swagger
    failMethodNotAllowed = callback $
      responseLBS status405 [(hContentType, cs "text/plain")] (cs $ "Unsupported HTTP method: " ++ reqMethod)
    failPathNotFound = callback $
      responseLBS status404 [(hContentType, cs "text/plain")] (cs $ "Path not found: " ++ pathInfo)

