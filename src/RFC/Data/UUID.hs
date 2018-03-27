{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module RFC.Data.UUID
  ( module Data.UUID.Types
  ) where

import           ClassyPrelude       hiding (fail)
import           Control.Monad.Fail  (MonadFail, fail)
import           RFC.String

import           Data.UUID.Types     hiding (fromString)
import qualified Data.UUID.Types     as UUID

#if MIN_VERSION_aeson(1,1,0)
-- UUID has ToJSON and FromJSON
#else
import           Data.Aeson.Types    (FromJSON (..), ToJSON (..),
                                      Value (String), typeMismatch)
import qualified Data.Text           as T
#endif

#ifndef GHCJS_BROWSER
import           Servant.API.Capture
import           Servant.Docs
#endif

#ifndef GHCJS_BROWSER
instance ToCapture (Capture "id" UUID) where
  toCapture _ = DocCapture "id" "UUID identifier"

instance ToSample UUID where
  toSamples _ = samples . catMaybes $ UUID.fromString <$>
    [ "cf41ac06-3f70-479c-a2ed-d618a5e6dee2"
    , "26998bb3-d6c6-4f63-8a36-6b81eb6e6de9"
    , "6176b857-e461-4f34-a6a6-aeb8cbf7ffdf"
    , "26009820-d2d1-4360-87e0-aa73db3c0433"
    ]

#endif

#if MIN_VERSION_aeson(1,1,0)
-- UUID has ToJSON and FromJSON
#else

instance ToJSON UUID where
  toJSON = String . T.pack . show
  {-# INLINE toJSON #-}

instance FromJSON UUID where
  parseJSON json@(String t) =
    case UUID.fromText t of
         Just uuid -> pure uuid
         Nothing   -> typeMismatch "UUID" json
  parseJSON unknown = typeMismatch "UUID" unknown
  {-# INLINE parseJSON #-}

#endif

instance {-# OVERLAPPING #-} ToText UUID where
  toText = UUID.toText
  {-# INLINE toText #-}

instance {-# OVERLAPS #-} (MonadFail m) => FromText (m UUID) where
  {-# SPECIALISE instance FromText (Maybe UUID) #-}
  {-# SPECIALISE instance FromText ([UUID])     #-}
  {-# SPECIALISE instance FromText (IO UUID)    #-}
  fromText :: Text -> m UUID
  fromText text =
    case UUID.fromText text of
      Nothing -> fail $ "Could not parse UUID: " <> (cs text)
      Just x  -> return x
  {-# INLINE fromText #-}

instance {-# OVERLAPPING #-} ConvertibleStrings UUID String where
  cs = UUID.toString
  {-# INLINE cs #-}

instance {-# OVERLAPPING #-} ConvertibleStrings UUID StrictText where
  cs = UUID.toText
  {-# INLINE cs #-}

instance {-# OVERLAPPING #-} ConvertibleStrings UUID (UTF8 StrictByteString) where
  cs = UTF8 . UUID.toASCIIBytes
  {-# INLINE cs #-}

instance {-# OVERLAPPING #-} ConvertibleStrings UUID (UTF8 LazyByteString) where
  cs = UTF8 . UUID.toLazyASCIIBytes
  {-# INLINE cs #-}

