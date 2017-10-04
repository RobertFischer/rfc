module RFC.Data.LatLng
  ( LatLng(..)
  , Longitude
  , Latitude
  , latLng
  , lngLat
  , longitude
  , latitude
  ) where

import RFC.Prelude
import RFC.Psql as Psql
import RFC.JSON as JSON

type Latitude = Double
type Longitude = Double

newtype LatLng = LatLng (Latitude,Longitude) deriving (Eq, Ord, Show, Typeable, Generic)
$(JSON.deriveJSON JSON.jsonOptions ''LatLng)

instance FromRow LatLng where
  fromRow = latLng <$> Psql.field <*> Psql.field

instance FromRow (Maybe LatLng) where
  fromRow = Just <$> fromRow

latLng :: Latitude -> Longitude -> LatLng
latLng lat lng = LatLng (lat,lng)

lngLat :: Longitude -> Latitude -> LatLng
lngLat = flip latLng

longitude :: LatLng -> Longitude
longitude (LatLng(_,l)) = l

latitude :: LatLng -> Latitude
latitude (LatLng(l,_)) = l
