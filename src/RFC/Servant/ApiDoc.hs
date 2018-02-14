{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE UndecidableInstances  #-}

module RFC.Servant.ApiDoc
  ( apiToHtml
  , apiToAscii
  , apiToSwagger
  , apiMiddleware
  , swaggerSchemaOptions
  , ToSchemaRFC
  ) where

import qualified Data.Aeson                      as Aeson
import           Data.Aeson.Types                (fromEncoding, toEncoding)
import qualified Data.Binary.Builder             as Builder
import           Data.Char                       as Char
import           Data.Default                    (def)
import           Data.Monoid                     ((<>))
import           Data.Swagger
import           Data.Swagger.Declare
import           Data.Swagger.Internal.Schema    (GToSchema)
import           Data.Swagger.Internal.TypeShape (TypeHasSimpleShape)
import           GHC.Generics                    (Rep)
import           Network.HTTP.Types.Header       (hContentType)
import           Network.HTTP.Types.Status
import           Network.Wai
import           RFC.JSON                        (jsonOptions)
import           RFC.Prelude                     hiding ((<>))
import           RFC.Servant
import           RFC.String                      ()
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

apiMiddleware :: (HasDocs a, HasSwagger a) => Proxy a -> Swagger -> Application -> Application
apiMiddleware api addlSwagger application request callback =
  case (reqMethod, reqPath) of
    ("GET", Just doIt) -> doIt
    _                  -> application request callback
  where
    html = Blaze.renderHtml $ apiToHtml api
    ascii :: LazyByteString
    ascii = apiToAscii api
    swaggerToLbs :: Swagger -> LazyByteString
    swaggerToLbs = Builder.toLazyByteString . fromEncoding . toEncoding
    swagger = swaggerToLbs $ apiToSwagger api <> addlSwagger
    reqMethod :: String
    reqMethod = map Char.toUpper $ cs $ requestMethod request
    pathInfo :: String
    pathInfo = map Char.toLower $ cs $ rawPathInfo request
    reqPath =
      case pathInfo of
        "swagger.json"  -> Just serveSwagger
        "/swagger.json" -> Just serveSwagger
        "api.html"      -> Just serveHtml
        "/api.html"     -> Just serveHtml
        "api.txt"       -> Just serveTxt
        "/api.txt"      -> Just serveTxt
        _               -> Nothing
    response ::
      (ConvertibleStrings contentType StrictByteString, ConvertibleStrings body LazyByteString) =>
      contentType -> body -> IO ResponseReceived
    response contentType body = callback $
      responseLBS status200 [(hContentType, cs contentType)] (cs body)
    serveHtml = response "text/html" html
    serveTxt = response "text/plain" ascii
    serveSwagger = response "application/json" swagger


instance ToSample () where
  toSamples _ = [(cs "No value", ())]

swaggerSchemaOptions :: SchemaOptions
swaggerSchemaOptions = SchemaOptions
  { fieldLabelModifier = Aeson.fieldLabelModifier jsonOptions
  , constructorTagModifier = Aeson.constructorTagModifier jsonOptions
  , unwrapUnaryRecords = Aeson.unwrapUnaryRecords jsonOptions
  , datatypeNameModifier = id
  , allNullaryToStringTag = True
  }

class ToSchemaRFC a where
  declareNamedSchemaRFC :: proxy a -> Declare (Definitions Schema) NamedSchema
  default declareNamedSchemaRFC ::
    (Generic a
    , GToSchema (Rep a)
    , TypeHasSimpleShape a "genericDeclareNamedSchemaUnrestricted"
    ) => proxy a -> Declare (Definitions Schema) NamedSchema
  declareNamedSchemaRFC = genericDeclareNamedSchema swaggerSchemaOptions

instance {-# OVERLAPPABLE #-} ToSchemaRFC a => ToSchema a where
  declareNamedSchema = declareNamedSchemaRFC
