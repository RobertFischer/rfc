{-# LANGUAGE CPP #-}

module RFC.Prelude.JSON
( jsonOptions
, deriveJSON
, FromJSON(..)
, ToJSON(..)
, eitherDecode
, decodeEither
, eitherDecode'
, decodeEither'
, decodeOrDie
, DecodeError
, Value(..)
, encode
, decode
, module Data.Aeson.Types
) where

import           ClassyPrelude
import           Data.Aeson                 as JSON
import           Data.Aeson.Parser          as JSONParser
import           Data.Aeson.TH              ( deriveJSON )
import           Data.Aeson.Types           ( Options (..), SumEncoding (..), Value (..) )
import           Data.Char
import           RFC.Prelude.String
import           Web.HttpApiData

-- How we go about executing the parser
#ifdef VERSION_aeson
#if MIN_VERSION_aeson(1,0,0)
import           Data.Aeson.Text            as JSON
import qualified Data.Aeson.Types           as JSONTypes
#else
import qualified Data.Aeson.Encode          as JSON
import           Data.Attoparsec.ByteString as JSON
import           Data.Either                ( either )
#endif
#endif

#ifdef VERSION_swagger2
import qualified Data.Swagger               as Swag
#endif

jsonOptions :: Options
jsonOptions = defaultOptions
    { sumEncoding = ObjectWithSingleField
    , unwrapUnaryRecords = True
    , fieldLabelModifier = flm
    , constructorTagModifier = ctm
    }
  where
    ctm []     = []
    ctm (c:cs) = (charToLower c):cs
    flm = flm' . span isLower
    flm' (cs, []) = cs
    flm' (_, cs)  = lowerFirst cs
    lowerFirst []     = []
    lowerFirst (c:cs) = (charToLower c):cs

decodeEither :: (FromJSON a) => LazyByteString -> Either String a
decodeEither = eitherDecode

decodeEither' :: (FromJSON a) => LazyByteString -> Either String a
decodeEither' = eitherDecode'

#ifdef VERSION_aeson
newtype DecodeError = DecodeError (LazyByteString, String) deriving (Show,Eq,Ord,Generic,Typeable)
instance Exception DecodeError

decodeOrDie :: (FromJSON a, MonadIO  m) => LazyByteString -> m a
decodeOrDie input =
  case decodeEither' input of
    Left err -> throwIO $ DecodeError (input, err)
    Right a  -> return a

instance FromHttpApiData JSON.Value where
  parseUrlPiece text =
      case parsed of
        Nothing      -> Left $ (toText "Could not parse JSON: ") <> text
        (Just value) -> Right value
    where
      textBs = toUTF8 text
      parser = JSONParser.value'
      parsed =
#if MIN_VERSION_aeson(1,0,0)
          JSONParser.decodeStrictWith parser JSONTypes.Success textBs
#else
          either (const Nothing) Just $ JSON.parseOnly parser textBs
#endif

instance ToHttpApiData JSON.Value where
  toUrlPiece =
#if MIN_VERSION_aeson(1,0,0)
    fromText . toText . JSON.encodeToLazyText
#else
    fromText . toText . JSON.encodeToTextBuilder
#endif

#ifdef VERSION_swagger2
instance Swag.ToSchema Value where
  declareNamedSchema _ = return . Swag.NamedSchema (Just $ cs "Value") $ mempty
#endif

#endif
