{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

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

type LazyText = LT.Text
type StrictText = ST.Text
type LazyByteString = LB.ByteString
type StrictByteString = SB.ByteString

type ConvertibleString       = ConvertibleStrings
type ConvertibleToSBS a      = ConvertibleStrings a StrictByteString
type ConvertibleFromSBS a    = ConvertibleStrings StrictByteString a
type ConvertibleToString a   = ConvertibleStrings a String
type ConvertibleFromString a = ConvertibleStrings String a

toLazyText :: (ConvertibleStrings a LazyText) => a -> LazyText
toLazyText = cs
{-# INLINE toLazyText #-}
{-# SPECIALIZE INLINE toLazyText :: String -> LazyText #-}
{-# SPECIALIZE INLINE toLazyText :: StrictText -> LazyText #-}

instance {-# OVERLAPPING #-} ToText URI where
  toText uri = cs $ uriToString id uri ""
  {-# INLINE toText #-}

instance {-# OVERLAPS #-} (MonadFail f) => FromText (f URI) where
  {-# SPECIALIZE instance FromText (Maybe URI) #-}
  {-# SPECIALIZE instance FromText (IO URI)    #-}
  fromText txt =
    case parseURIReference (cs txt) of
      Nothing  -> fail $ "Could not parse URI: " ++ (cs txt)
      Just uri -> return uri
  {-# INLINE fromText #-}

class ConvertibleStrings a b where
  cs :: a -> b

instance {-# INCOHERENT #-} (ToText a, FromText b) => ConvertibleStrings a b where
  {-# SPECIALIZE instance ConvertibleStrings String LazyText                              #-}
  {-# SPECIALIZE instance ConvertibleStrings String StrictText                            #-}
  {-# SPECIALIZE instance ConvertibleStrings LazyText String                              #-}
  {-# SPECIALIZE instance ConvertibleStrings LazyText StrictText                          #-}
  {-# SPECIALIZE instance ConvertibleStrings StrictText LazyText                          #-}
  {-# SPECIALIZE instance ConvertibleStrings String (UTF8 StrictByteString)               #-}
  {-# SPECIALIZE instance ConvertibleStrings StrictText (UTF8 StrictByteString)           #-}
  {-# SPECIALIZE instance ConvertibleStrings LazyText (UTF8 StrictByteString)             #-}
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
  {-# SPECIALIZE instance ConvertibleStrings String (UTF8 LazyByteString)                 #-}
  {-# SPECIALIZE instance ConvertibleStrings StrictText (UTF8 LazyByteString)             #-}
  {-# SPECIALIZE instance ConvertibleStrings LazyText (UTF8 LazyByteString)               #-}
  {-# SPECIALIZE instance ConvertibleStrings StrictText (Maybe (Base64 LazyByteString))   #-}
  {-# SPECIALIZE instance ConvertibleStrings LazyText (Maybe (Base64 LazyByteString))     #-}
  {-# SPECIALIZE instance ConvertibleStrings String (Maybe (Base64 LazyByteString))       #-}
  {-# SPECIALIZE instance ConvertibleStrings StrictText (Maybe (Base16 LazyByteString))   #-}
  {-# SPECIALIZE instance ConvertibleStrings LazyText (Maybe (Base16 LazyByteString))     #-}
  {-# SPECIALIZE instance ConvertibleStrings String (Maybe (Base16 LazyByteString))       #-}
  cs :: a -> b
  cs = fromText . toText
  {-# INLINE cs #-}

instance {-# OVERLAPPING #-} ConvertibleStrings StrictText String where
  cs :: StrictText -> String
  cs = ST.unpack
  {-# INLINE cs #-}

instance {-# OVERLAPPING #-} ConvertibleStrings a a where
  cs :: a -> a
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

