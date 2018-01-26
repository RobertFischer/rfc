{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module RFC.Data.UUID
  ( module Data.UUID.Types
  ) where

import           Data.UUID.Types
import qualified Data.UUID.Types                    as UUID
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.Types   (Only (..))
import           Prelude                            (String, return, ($))
import           RFC.String

instance FromRow UUID where
  fromRow = do
    (Only id) <- fromRow
    return id

instance ToRow UUID where
  toRow id = toRow $ Only id

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
