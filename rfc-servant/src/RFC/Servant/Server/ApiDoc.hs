{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE UndecidableInstances  #-}

module RFC.Servant.Server.ApiDoc
  ( apiToHtml
  , apiToAscii
  , apiToSwagger
  , apiMiddleware
  , swaggerSchemaOptions
  , ToSchemaRFC
  , module Data.Swagger
  , module Servant.Swagger.UI
  , module Servant.Swagger
  ) where

import           Control.Lens
import qualified Data.Aeson                      as Aeson
import           Data.Aeson.Types                ( fromEncoding, toEncoding )
import qualified Data.Binary.Builder             as Builder
import           Data.Default                    ( def )
import           Data.Monoid                     ( (<>) )
import           Data.Swagger
import           Data.Swagger.Declare
import           Data.Swagger.Internal.Schema    ( GToSchema )
import           Data.Swagger.Internal.TypeShape ( TypeHasSimpleShape )
import           GHC.Generics                    ( Rep )
import           Network.HTTP.Types.Header       ( hContentType )
import           Network.HTTP.Types.Status
import           Network.Wai
import           RFC.Prelude                     hiding ( (<>) )
import           RFC.Servant.Server
import           Servant.Swagger
import           Servant.Swagger.UI
import qualified Text.Blaze.Html.Renderer.String as Blaze
import qualified Text.Markdown                   as MD

apiToHtml :: (HasDocs a) => Proxy a -> Html
apiToHtml = preEscapedToHtml . (MD.markdown mdSettings) . toLazyText . markdown . docs
  where
    mdSettings = def
      { MD.msLinkNewTab = False
      , MD.msAddHeadingId = True
      }

apiToAscii :: HasDocs a => Proxy a -> String
apiToAscii = markdown . docs

apiToSwagger :: (HasSwagger a) => Proxy a -> Swagger
apiToSwagger = toSwagger

apiMiddleware :: (HasDocs a, HasSwagger a) => Proxy a -> Swagger -> Application -> Application
apiMiddleware api addlSwagger application request callback =
  case (reqMethod, reqPath) of
    ("GET", Just doIt) -> doIt
    _                  -> application request callback
  where
    bsToStr :: StrictByteString -> String
    bsToStr = fromMaybe "" . fromUTF8
    html = Blaze.renderHtml $ apiToHtml api
    ascii = apiToAscii api
    swaggerToLbs = Builder.toLazyByteString . fromEncoding . toEncoding
    swagger = swaggerToLbs $ apiToSwagger api <> addlSwagger
    reqMethod = fmap charToUpper . bsToStr $ requestMethod request
    pathInfo = fmap charToLower . bsToStr $ rawPathInfo request
    reqPath =
      case pathInfo of
        "swagger.json"  -> Just serveSwagger
        "/swagger.json" -> Just serveSwagger
        "api.html"      -> Just serveHtml
        "/api.html"     -> Just serveHtml
        "api.txt"       -> Just serveTxt
        "/api.txt"      -> Just serveTxt
        _               -> Nothing
    response contentType body = callback $
      responseLBS status200 [(hContentType, contentType)] body
    serveHtml = response (toUTF8 "text/html") (toUTF8 html)
    serveTxt = response (toUTF8 "text/plain") (toUTF8 ascii)
    serveSwagger = response (toUTF8 "application/json") swagger


instance ToSample () where
  toSamples _ = [(toText "No value", ())]

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

instance {-# OVERLAPPABLE #-} ToSchema (Ratio a) where
  declareNamedSchema _ = do
    integerSchema <- declareSchemaRef (Proxy :: Proxy Integer)
    return . NamedSchema (Just . toText $ "Ratio") $ mempty
      & type_ .~ SwaggerObject
      & properties .~ (fromList
        [ (toText "numerator", integerSchema)
        , (toText "denominator", integerSchema)
        ])
      & required .~ (toText <$> [ "numerator", "denominator" ])

instance {-# OVERLAPPING #-} ToSample Int16 where
    toSamples _ =  [] -- TODO Impl this
