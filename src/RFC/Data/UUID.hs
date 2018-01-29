{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module RFC.Data.UUID
  ( module Data.UUID.Types
  ) where

import           ClassyPrelude
import           RFC.String

import           Data.UUID.Types
import qualified Data.UUID.Types                    as UUID

#ifndef GHCJS_BROWSER
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.Types   (Only (..))
#endif

#ifndef GHCJS_BROWSER
instance FromRow UUID where
  fromRow = do
    (Only id) <- fromRow
    return id

instance ToRow UUID where
  toRow id = toRow $ Only id
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
