module RFC.Data.IdAnd
  ( idAndsToMap
  , IdAnd(..)
  , valuesToIdAnd
  , idAndToTuple
  , tupleToIdAnd
  ) where

import RFC.Prelude
import RFC.JSON
import Data.Aeson as JSON
import Data.Map as Map
import Data.List as List hiding ((++))
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField()

-- |Represents something which has an ID.
newtype IdAnd a = IdAnd (UUID, a)
  deriving (Eq, Ord, Read, Show, Generic, Typeable)

tupleToIdAnd :: (UUID, a) -> IdAnd a
tupleToIdAnd = IdAnd

valuesToIdAnd :: UUID -> a -> IdAnd a
valuesToIdAnd id a = IdAnd(id,a)

idAndToTuple :: IdAnd a -> (UUID, a)
idAndToTuple (IdAnd it) = it

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
