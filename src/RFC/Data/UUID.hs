{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module RFC.Data.UUID
  ( module Data.UUID.Types
  ) where

import           ClassyPrelude
import           RFC.String

import           Data.UUID.Types
import qualified Data.UUID.Types                    as UUID

#if MIN_VERSION_aeson(1,1,0)
-- UUID has ToJSON and FromJSON
#else
import           Data.Aeson.Types
#endif

#ifndef GHCJS_BROWSER
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.Types   (Only (..))
import           Servant.API.Capture
import           Servant.Docs
#endif

#ifndef GHCJS_BROWSER
instance ToCapture (Capture "id" UUID) where
  toCapture _ = DocCapture "id" "UUID identifier"

instance ToSample UUID where
  toSamples _ = samples $ catMaybes $ map UUID.fromString $
    [ "cf41ac06-3f70-479c-a2ed-d618a5e6dee2"
    , "26998bb3-d6c6-4f63-8a36-6b81eb6e6de9"
    , "6176b857-e461-4f34-a6a6-aeb8cbf7ffdf"
    , "26009820-d2d1-4360-87e0-aa73db3c0433"
    ]

#endif

#ifndef GHCJS_BROWSER
instance FromRow UUID where
  fromRow = do
    (Only id) <- fromRow
    return id

instance ToRow UUID where
  toRow id = toRow $ Only id
#endif

#if MIN_VERSION_aeson(1,1,0)
-- UUID has ToJSON and FromJSON
#else

instance ToJSON UUID where
  toJSON = toJSON . UUID.toText

instance FromJSON UUID where
  parseJSON (String txt) =
    case UUID.fromText txt of
      Nothing   -> fail $ "Invalid UUID: " ++ cs txt
      Just uuid -> return uuid

  parseJSON invalid = typeMismatch "UUID" invalid

#endif

instance ConvertibleStrings UUID String where
  convertString = UUID.toString

instance ConvertibleStrings UUID StrictText where
  convertString = UUID.toText

instance ConvertibleStrings UUID LazyText where
  convertString = toLazyText

instance ConvertibleStrings UUID StrictByteString where
  convertString = UUID.toASCIIBytes

instance ConvertibleStrings UUID LazyByteString where
  convertString = UUID.toLazyASCIIBytes

