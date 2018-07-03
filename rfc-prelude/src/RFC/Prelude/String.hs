{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}


module RFC.Prelude.String
  ( module RFC.Prelude.String
  , module Data.Text.Conversions
  ) where

import           ClassyPrelude          hiding ( fail )
import           Control.Monad.Fail     ( MonadFail, fail )
import qualified Data.ByteString        as SB
import qualified Data.ByteString.Lazy   as LB
import qualified Data.ByteString.Short  as Sbs
import           Data.String            ( String )
import qualified Data.Text              as ST
import           Data.Text.Conversions
import qualified Data.Text.Lazy         as LT
import qualified Data.Text.Lazy.Builder as LTBuilder
import           Network.URI            ( URI (..), parseURIReference, uriToString )
import           RFC.Prelude.Instances  ()

#if VERSION_servant_docs
import           Servant.Docs
#endif

{-# ANN module "HLint: ignore Use String" #-}

type LazyText = LT.Text
type StrictText = ST.Text
type LazyByteString = LB.ByteString
type StrictByteString = SB.ByteString
type ShortByteString = Sbs.ShortByteString
type LazyTextBuilder = LTBuilder.Builder

toString :: (ToText a) => a -> String
toString = ST.unpack . toText
{-# INLINE toString #-}
{-# SPECIALIZE INLINE toString :: String -> String     #-}
{-# SPECIALIZE INLINE toString :: LazyText -> String   #-}
{-# SPECIALIZE INLINE toString :: StrictText -> String #-}

toStrictText :: (ToText a) => a -> StrictText
toStrictText = toText
{-# INLINE toStrictText #-}
{-# SPECIALIZE INLINE toStrictText :: String -> StrictText     #-}
{-# SPECIALIZE INLINE toStrictText :: LazyText -> StrictText   #-}
{-# SPECIALIZE INLINE toStrictText :: StrictText -> StrictText #-}

toLazyText :: (ToText a) => a -> LazyText
toLazyText = LT.fromStrict . toText
{-# INLINE toLazyText #-}
{-# SPECIALIZE INLINE toLazyText :: String -> LazyText     #-}
{-# SPECIALIZE INLINE toLazyText :: StrictText -> LazyText #-}
{-# SPECIALIZE INLINE toLazyText :: LazyText -> LazyText   #-}

toUTF8 :: (ToText a, FromText (UTF8 b)) => a -> b
toUTF8 = unUTF8 . fromText . toText
{-# INLINE toUTF8 #-}
{-# SPECIALIZE INLINE toUTF8 :: String-> LazyByteString          #-}
{-# SPECIALIZE INLINE toUTF8 :: StrictText -> LazyByteString     #-}
{-# SPECIALIZE INLINE toUTF8 :: LazyText -> LazyByteString       #-}
{-# SPECIALIZE INLINE toUTF8 :: String-> StrictByteString        #-}
{-# SPECIALIZE INLINE toUTF8 :: StrictText -> StrictByteString   #-}
{-# SPECIALIZE INLINE toUTF8 :: LazyText -> StrictByteString     #-}
{-# SPECIALIZE INLINE toUTF8 :: String-> ShortByteString         #-}
{-# SPECIALIZE INLINE toUTF8 :: StrictText -> ShortByteString    #-}
{-# SPECIALIZE INLINE toUTF8 :: LazyText -> ShortByteString      #-}

fromUTF8 :: (DecodeText f (UTF8 a), FromText b) => a -> f b
fromUTF8 = decodeConvertText . UTF8
{-# INLINE fromUTF8 #-}
{-# SPECIALIZE INLINE fromUTF8 :: LazyByteString -> Maybe String       #-}
{-# SPECIALIZE INLINE fromUTF8 :: LazyByteString -> Maybe StrictText   #-}
{-# SPECIALIZE INLINE fromUTF8 :: LazyByteString -> Maybe LazyText     #-}
{-# SPECIALIZE INLINE fromUTF8 :: StrictByteString -> Maybe String     #-}
{-# SPECIALIZE INLINE fromUTF8 :: StrictByteString -> Maybe StrictText #-}
{-# SPECIALIZE INLINE fromUTF8 :: StrictByteString -> Maybe LazyText   #-}
{-# SPECIALIZE INLINE fromUTF8 :: ShortByteString -> Maybe String      #-}
{-# SPECIALIZE INLINE fromUTF8 :: ShortByteString -> Maybe StrictText  #-}
{-# SPECIALIZE INLINE fromUTF8 :: ShortByteString -> Maybe LazyText    #-}

instance {-# OVERLAPPING #-} FromText (UTF8 ShortByteString) where
  fromText = UTF8 . Sbs.toShort . encodeUtf8
  {-# INLINE fromText #-}

instance {-# INCOHERENT #-} (MonadFail m, Show (UTF8 a), DecodeText Maybe (UTF8 a)) => DecodeText m (UTF8 a) where
  {-# SPECIALIZE instance DecodeText IO (UTF8 ShortByteString)       #-}
  {-# SPECIALIZE instance DecodeText [] (UTF8 ShortByteString)       #-}
  decodeText arg =
    case decodeText arg of
      Nothing ->
        fail $ "Unable to decode text: " <> show arg
      Just x ->
        return x
  {-# INLINE decodeText #-}

emptyString :: (FromText a) => a
emptyString = fromText $ toText ""
{-# INLINE emptyString #-}
{-# SPECIALIZE INLINE emptyString :: String     #-}
{-# SPECIALIZE INLINE emptyString :: LazyText   #-}
{-# SPECIALIZE INLINE emptyString :: StrictText #-}

emptyUTF8 :: (FromText (UTF8 a)) => a
emptyUTF8 = unUTF8 . fromText $ toText ""
{-# INLINE emptyUTF8 #-}
{-# SPECIALIZE INLINE emptyUTF8 :: LazyByteString #-}
{-# SPECIALIZE INLINE emptyUTF8 :: StrictByteString #-}

instance {-# OVERLAPPING #-} ToText Char where
  toText c = toText [c]
  {-# INLINE toText #-}

instance {-# OVERLAPPABLE #-} (FromText a) => FromText (UTF8 a) where
  {-# SPECIALISE instance FromText (UTF8 StrictText) #-}
  {-# SPECIALISE instance FromText (UTF8 LazyText)   #-}
  {-# SPECIALISE instance FromText (UTF8 String)     #-}
  fromText = UTF8 . fromText
  {-# INLINE fromText #-}

instance {-# OVERLAPPABLE #-} (ToText a) => ToText (UTF8 a) where
  {-# SPECIALISE instance ToText (UTF8 StrictText) #-}
  {-# SPECIALISE instance ToText (UTF8 LazyText)   #-}
  {-# SPECIALISE instance ToText (UTF8 String)     #-}
  toText = toText . unUTF8
  {-# INLINE toText #-}

instance {-# OVERLAPPING #-} ToText URI where
  toText uri = toText $ uriToString id uri ""
  {-# INLINE toText #-}

instance {-# OVERLAPS #-} (MonadFail f) => FromText (f URI) where
  {-# SPECIALIZE instance FromText (Maybe URI) #-}
  {-# SPECIALIZE instance FromText (IO URI)    #-}
  fromText txt =
    let str = fromText txt in
    case parseURIReference str of
      Nothing  -> fail $ "Could not parse URI: " <> str
      Just uri -> return uri
  {-# INLINE fromText #-}

#if VERSION_servant_docs

instance ToSample StrictText where
  toSamples _ = singleSample $ toText "This is arbitrary text"

instance ToSample LazyText where
  toSamples _ = singleSample $ toText "This is arbitrary text"

#endif


instance {-# OVERLAPPING #-} ToText LazyTextBuilder where
  toText = toText . LTBuilder.toLazyText
  {-# INLINE toText #-}

