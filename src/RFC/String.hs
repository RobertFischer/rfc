{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}


module RFC.String
  ( module RFC.String
  , module Data.Text.Conversions
  ) where

import           ClassyPrelude          hiding (fail)
import           Control.Monad.Fail     (MonadFail, fail)
import qualified Data.ByteString        as SB
import qualified Data.ByteString.Lazy   as LB
import           Data.String            (String)
import qualified Data.Text              as ST
import           Data.Text.Conversions
import qualified Data.Text.Lazy         as LT
import qualified Data.Text.Lazy.Builder as LTBuilder
import           Network.URI            (URI (..), parseURIReference,
                                         uriToString)

#ifndef GHCJS_BROWSER
import           Servant.Docs
#endif

{-# ANN module "HLint: ignore Use String" #-}

type LazyText = LT.Text
type StrictText = ST.Text
type LazyByteString = LB.ByteString
type StrictByteString = SB.ByteString

type ConvertibleString       = ConvertibleStrings
type ConvertibleToSBS a      = ConvertibleStrings a StrictByteString
type ConvertibleFromSBS a    = ConvertibleStrings StrictByteString a
type ConvertibleToString a   = ConvertibleStrings a String
type ConvertibleFromString a = ConvertibleStrings String a

toStrictText :: (ToText a) => a -> StrictText
toStrictText = toText
{-# SPECIALIZE INLINE toStrictText :: String -> StrictText #-}
{-# SPECIALIZE INLINE toStrictText :: LazyText -> StrictText #-}
{-# SPECIALIZE INLINE toStrictText :: StrictText -> StrictText #-}

toLazyText :: (ToText a) => a -> LazyText
toLazyText = LT.fromStrict . toText
{-# INLINE toLazyText #-}
{-# SPECIALIZE INLINE toLazyText :: String -> LazyText     #-}
{-# SPECIALIZE INLINE toLazyText :: StrictText -> LazyText #-}
{-# SPECIALIZE INLINE toLazyText :: LazyText -> LazyText #-}

asUTF8 :: (ToText a, FromText (UTF8 b)) => a -> b
asUTF8 it = unUTF8 . fromText $ toText it
{-# INLINE asUTF8 #-}
{-# SPECIALIZE INLINE asUTF8 :: String-> LazyByteString   #-}
{-# SPECIALIZE INLINE asUTF8 :: StrictText -> LazyByteString   #-}
{-# SPECIALIZE INLINE asUTF8 :: LazyText -> LazyByteString   #-}
{-# SPECIALIZE INLINE asUTF8 :: String-> StrictByteString   #-}
{-# SPECIALIZE INLINE asUTF8 :: StrictText -> StrictByteString   #-}
{-# SPECIALIZE INLINE asUTF8 :: LazyText -> StrictByteString   #-}

toUTF8 :: (DecodeText f (UTF8 a), FromText b) => a -> f b
toUTF8 it = decodeConvertText (UTF8 it)
{-# INLINE toUTF8 #-}
{-# SPECIALIZE INLINE toUTF8 :: LazyByteString -> Maybe String #-}
{-# SPECIALIZE INLINE toUTF8 :: LazyByteString -> Maybe StrictText #-}
{-# SPECIALIZE INLINE toUTF8 :: LazyByteString -> Maybe LazyText #-}
{-# SPECIALIZE INLINE toUTF8 :: StrictByteString -> Maybe String #-}
{-# SPECIALIZE INLINE toUTF8 :: StrictByteString -> Maybe StrictText #-}
{-# SPECIALIZE INLINE toUTF8 :: StrictByteString -> Maybe LazyText #-}

instance {-# OVERLAPPABLE #-} (Show a, DecodeText Maybe a, MonadFail m) => DecodeText m a where
  {-# SPECIALIZE instance DecodeText IO (UTF8 LazyByteString)    #-}
  {-# SPECIALIZE instance DecodeText IO (UTF8 StrictByteString)  #-}
  {-# SPECIALIZE instance DecodeText [] (UTF8 LazyByteString)    #-}
  {-# SPECIALIZE instance DecodeText [] (UTF8 StrictByteString)  #-}
  {-# SPECIALIZE instance DecodeText Maybe (UTF8 LazyByteString)    #-}
  {-# SPECIALIZE instance DecodeText Maybe (UTF8 StrictByteString)  #-}
  decodeText a =
    case decodeText a of
      [] ->
        fail $ "Could not decode text: " <> (show a)
      x:_ ->
        return x

emptyString :: (FromText a) => a
emptyString = fromText $ toText ""
{-# INLINE emptyString #-}
{-# SPECIALIZE INLINE emptyString :: String     #-}
{-# SPECIALIZE INLINE emptyString :: LazyText   #-}
{-# SPECIALIZE INLINE emptyString :: StrictText #-}

emptyUTF8 :: (FromText (UTF8 a)) => a
emptyUTF8 = unUTF8 . fromText $ toText ""
{-# INLINE emptyUTF8 #-}

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
    case parseURIReference (fromText txt) of
      Nothing  -> fail $ "Could not parse URI: " <> (cs txt)
      Just uri -> return uri
  {-# INLINE fromText #-}

class ConvertibleStrings a b where
  cs :: a -> b

instance {-# OVERLAPPING #-} ConvertibleStrings LazyByteString StrictByteString where
  cs = LB.toStrict
  {-# INLINE cs #-}

instance {-# OVERLAPPING #-} ConvertibleStrings StrictByteString LazyByteString where
  cs = LB.fromStrict
  {-# INLINE cs #-}

instance {-# OVERLAPPABLE #-} (DecodeText f a, FromText b) => ConvertibleStrings a (f b) where
  cs = decodeConvertText
  {-# INLINE cs #-}

instance {-# OVERLAPS #-} (ToText (UTF8 a), FromText b) => ConvertibleStrings a (UTF8 b) where
  cs a = UTF8 . fromText . toText $ UTF8 a
  {-# INLINE cs #-}

instance {-# OVERLAPS #-} (ToText a, FromText (UTF8 b)) => ConvertibleStrings (UTF8 a) b where
  {-# SPECIALISE instance ConvertibleStrings (UTF8 StrictText) LazyByteString   #-}
  {-# SPECIALISE instance ConvertibleStrings (UTF8 LazyText) LazyByteString     #-}
  {-# SPECIALISE instance ConvertibleStrings (UTF8 String) LazyByteString       #-}
  {-# SPECIALISE instance ConvertibleStrings (UTF8 StrictText) StrictByteString #-}
  {-# SPECIALISE instance ConvertibleStrings (UTF8 LazyText) StrictByteString   #-}
  {-# SPECIALISE instance ConvertibleStrings (UTF8 String) StrictByteString     #-}
  cs (UTF8 it) = unUTF8 . fromText $ toText it
  {-# INLINE cs #-}

instance {-# OVERLAPS #-} (ToText a, FromText b) => ConvertibleStrings a b where
  {-# SPECIALIZE instance ConvertibleStrings String LazyText                              #-}
  {-# SPECIALIZE instance ConvertibleStrings String StrictText                            #-}
  {-# SPECIALIZE instance ConvertibleStrings LazyText String                              #-}
  {-# SPECIALIZE instance ConvertibleStrings LazyText StrictText                          #-}
  {-# SPECIALIZE instance ConvertibleStrings StrictText LazyText                          #-}
  {-# SPECIALIZE instance ConvertibleStrings StrictText (Maybe (Base64 StrictByteString)) #-}
  {-# SPECIALIZE instance ConvertibleStrings LazyText (Maybe (Base64 StrictByteString))   #-}
  {-# SPECIALIZE instance ConvertibleStrings String (Maybe (Base64 StrictByteString))     #-}
  {-# SPECIALIZE instance ConvertibleStrings StrictText (Maybe (Base16 StrictByteString)) #-}
  {-# SPECIALIZE instance ConvertibleStrings LazyText (Maybe (Base16 StrictByteString))   #-}
  {-# SPECIALIZE instance ConvertibleStrings String (Maybe (Base16 StrictByteString))     #-}
  {-# SPECIALIZE instance ConvertibleStrings (Base16 StrictByteString) String             #-}
  {-# SPECIALIZE instance ConvertibleStrings (Base16 StrictByteString) StrictText         #-}
  {-# SPECIALIZE instance ConvertibleStrings (Base16 StrictByteString) LazyText           #-}
  {-# SPECIALIZE instance ConvertibleStrings (Base64 StrictByteString) String             #-}
  {-# SPECIALIZE instance ConvertibleStrings (Base64 StrictByteString) StrictText         #-}
  {-# SPECIALIZE instance ConvertibleStrings (Base64 StrictByteString) LazyText           #-}
  {-# SPECIALIZE instance ConvertibleStrings (Base16 LazyByteString) String               #-}
  {-# SPECIALIZE instance ConvertibleStrings (Base16 LazyByteString) StrictText           #-}
  {-# SPECIALIZE instance ConvertibleStrings (Base16 LazyByteString) LazyText             #-}
  {-# SPECIALIZE instance ConvertibleStrings (Base64 LazyByteString) String               #-}
  {-# SPECIALIZE instance ConvertibleStrings (Base64 LazyByteString) StrictText           #-}
  {-# SPECIALIZE instance ConvertibleStrings (Base64 LazyByteString) LazyText             #-}
  {-# SPECIALIZE instance ConvertibleStrings StrictText (Maybe (Base64 LazyByteString))   #-}
  {-# SPECIALIZE instance ConvertibleStrings LazyText (Maybe (Base64 LazyByteString))     #-}
  {-# SPECIALIZE instance ConvertibleStrings String (Maybe (Base64 LazyByteString))       #-}
  {-# SPECIALIZE instance ConvertibleStrings StrictText (Maybe (Base16 LazyByteString))   #-}
  {-# SPECIALIZE instance ConvertibleStrings LazyText (Maybe (Base16 LazyByteString))     #-}
  {-# SPECIALIZE instance ConvertibleStrings String (Maybe (Base16 LazyByteString))       #-}
  {-# SPECIALISE instance ConvertibleStrings StrictText (UTF8 LazyByteString)             #-}
  {-# SPECIALISE instance ConvertibleStrings LazyText (UTF8 LazyByteString)               #-}
  {-# SPECIALISE instance ConvertibleStrings String (UTF8 LazyByteString)                 #-}
  {-# SPECIALISE instance ConvertibleStrings StrictText (UTF8 StrictByteString)           #-}
  {-# SPECIALISE instance ConvertibleStrings LazyText (UTF8 StrictByteString)             #-}
  {-# SPECIALISE instance ConvertibleStrings String (UTF8 StrictByteString)               #-}
  cs :: a -> b
  cs = fromText . toText
  {-# INLINE cs #-}

instance {-# OVERLAPPING #-} ConvertibleStrings StrictText String where
  cs :: StrictText -> String
  cs = ST.unpack
  {-# INLINE cs #-}

instance {-# OVERLAPS #-} ConvertibleStrings a a where
  cs :: a -> a
  cs = id
  {-# INLINE cs #-}

instance {-# OVERLAPPING #-} ConvertibleStrings [Char] String where
  cs :: [Char] -> String
  cs = id
  {-# INLINE cs #-}

#ifndef GHCJS_BROWSER

instance ToSample StrictText where
  toSamples _ = singleSample $ cs "This is random text"

instance ToSample LazyText where
  toSamples _ = singleSample $ cs "This is random text"

#endif

type LazyTextBuilder = LTBuilder.Builder

instance {-# OVERLAPPING #-} ToText LazyTextBuilder where
  toText = cs . LTBuilder.toLazyText
  {-# INLINE toText #-}

