{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RFC.JSON
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
) where

import           Data.Aeson       as JSON
import           Data.Aeson.TH    (deriveJSON)
import           Data.Aeson.Types (Options (..), SumEncoding (..))
import           RFC.Prelude

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
    flm = flm' . span charIsLower
    flm' (cs, []) = cs
    flm' (_, cs)  = lowerFirst cs
    lowerFirst []     = []
    lowerFirst (c:cs) = (charToLower c):cs

decodeEither :: (FromJSON a) => LazyByteString -> Either String a
decodeEither = eitherDecode

decodeEither' :: (FromJSON a) => LazyByteString -> Either String a
decodeEither' = eitherDecode'

newtype DecodeError = DecodeError (LazyByteString, String) deriving (Show,Eq,Ord,Generic,Typeable)
instance Exception DecodeError

decodeOrDie :: (FromJSON a, MonadThrow m) => LazyByteString -> m a
decodeOrDie input =
  case decodeEither' input of
    Left err -> throwM $ DecodeError (input, err)
    Right a  -> return a
