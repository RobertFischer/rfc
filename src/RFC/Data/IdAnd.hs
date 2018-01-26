{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module RFC.Data.IdAnd
  ( idAndsToMap
  , IdAnd(..)
  , valuesToIdAnd
  , idAndToTuple
  , tupleToIdAnd
  , idAndToPair
  ) where

import           Data.Aeson                           as JSON
import           Data.List                            as List hiding ((++))
import           Data.Map                             as Map
import           Database.PostgreSQL.Simple.FromField ()
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           RFC.Prelude

-- |Represents something which has an ID.
newtype IdAnd a = IdAnd (UUID, a)
  deriving (Eq, Ord, Show, Generic, Typeable)

tupleToIdAnd :: (UUID, a) -> IdAnd a
tupleToIdAnd = IdAnd

valuesToIdAnd :: UUID -> a -> IdAnd a
valuesToIdAnd id a = IdAnd(id,a)

idAndToTuple :: IdAnd a -> (UUID, a)
idAndToTuple (IdAnd it) = it

idAndToPair :: IdAnd a -> (UUID, IdAnd a)
idAndToPair idAnd@(IdAnd (id,_)) = (id, idAnd)

idAndsToMap :: [IdAnd a] -> Map UUID a
idAndsToMap list = Map.fromList $ List.map idAndToTuple list

instance (FromJSON a) => FromJSON (IdAnd a) where
  parseJSON = JSON.withObject "IdAnd" $ \o -> do
    id <- o .: "id"
    value <- o .: "value"
    return $ IdAnd(id, value)

instance (ToJSON a) => ToJSON (IdAnd a) where
  toJSON (IdAnd (id,value)) = object [ "id".=id, "value".=value ]

instance (FromRow a) => FromRow (IdAnd a) where
  fromRow = valuesToIdAnd <$> field <*> fromRow

instance (ToRow a) => ToRow (IdAnd a) where
  toRow (IdAnd (id,a)) = toField id : toRow a
