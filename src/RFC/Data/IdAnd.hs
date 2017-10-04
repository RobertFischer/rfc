module RFC.Data.IdAnd
  ( idAndsToMap
  , IdAnd(..)
  , valuesToIdAnd
  , idAndToTuple
  ) where

import RFC.Prelude
import RFC.JSON
import Data.Aeson as JSON
import Data.Map as Map
import Data.List as List hiding ((++))
import Data.Vector as Vector hiding ((++))
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField()

-- |Represents something which has an ID.
newtype IdAnd a = IdAnd (UUID, a)
  deriving (Eq, Ord, Read, Show, Generic, Typeable)

valuesToIdAnd :: UUID -> a -> IdAnd a
valuesToIdAnd id a = IdAnd(id,a)

idAndToTuple :: IdAnd a -> (UUID, a)
idAndToTuple (IdAnd it) = it

idAndsToMap :: [IdAnd a] -> Map UUID a
idAndsToMap list = Map.fromList $ List.map idAndToTuple list

instance (FromJSON a) => FromJSON (IdAnd a) where
  parseJSON = JSON.withArray "IdAnd" $ \ary ->
    case Vector.length ary of
      2 -> do
        id <- indexM ary 0 >>= parseJSON
        a <- indexM ary 1 >>= parseJSON
        return $ IdAnd (id, a)
      x ->
        fail $ "Expected a 2-element array for IdAnd; got a " ++ (show x) ++ " length array instead."

instance (ToJSON a) => ToJSON (IdAnd a) where
  toJSON (IdAnd value) = toJSON value

instance (FromRow a) => FromRow (IdAnd a) where
  fromRow = valuesToIdAnd <$> field <*> fromRow

instance (ToRow a) => ToRow (IdAnd a) where
  toRow (IdAnd (id,a)) = toField id : toRow a
