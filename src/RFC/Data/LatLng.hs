module RFC.Data.LatLng
  ( LatLng(..)
  , Longitude
  , Latitude
  , latLng
  , lngLat
  ) where

import RFC.Prelude
import RFC.Psql as Psql
import RFC.JSON as JSON

type Latitude = Double
type Longitude = Double

data LatLng = LatLng {
  latitude :: Latitude,
  longitude :: Longitude
} deriving (Eq, Ord, Show, Typeable, Generic)
$(JSON.deriveJSON JSON.jsonOptions ''LatLng)

instance FromRow LatLng where
  fromRow = latLng <$> Psql.field <*> Psql.field

instance FromRow (Maybe LatLng) where
  fromRow = do
    parsed <- (fromRow :: RowParser (Maybe (Maybe Latitude, Maybe Longitude)))
    case parsed of
      (Just (Just lat, Just lng)) -> return $ Just $ latLng lat lng
      _ -> return Nothing


latLng :: Latitude -> Longitude -> LatLng
latLng lat lng = LatLng lat lng

lngLat :: Longitude -> Latitude -> LatLng
lngLat = flip latLng