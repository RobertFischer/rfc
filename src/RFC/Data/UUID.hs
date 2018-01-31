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
