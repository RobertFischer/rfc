{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}


module RFC.Data.LatLng
  ( LatLng(..)
  , Longitude
  , Latitude
  , latLng
  , lngLat
  ) where

import           RFC.JSON    as JSON
import           RFC.Prelude
#ifndef GHCJS_BROWSER
import           RFC.Psql    as Psql
#endif

type Latitude = Double
type Longitude = Double


data LatLng = LatLng {
  latitude  :: Latitude,
  longitude :: Longitude
} deriving (Eq, Ord, Show, Typeable, Generic)
$(JSON.deriveJSON JSON.jsonOptions ''LatLng)

#ifndef GHCJS_BROWSER
noLat :: Maybe Latitude
noLat = Nothing

noLng :: Maybe Longitude
noLng = Nothing

instance ToRow LatLng where
  toRow latLng = Psql.toRow (latitude latLng, longitude latLng)

instance ToRow (Maybe LatLng) where
  toRow maybeLatLng =
    case maybeLatLng of
      Nothing       -> map toField [noLat, noLng]
      (Just latLng) -> Psql.toRow latLng

instance FromRow LatLng where
  fromRow = latLng <$> Psql.field <*> Psql.field

instance FromRow (Maybe LatLng) where
  fromRow = do
    parsed <- (fromRow :: RowParser (Maybe (Maybe Latitude, Maybe Longitude)))
    case parsed of
      (Just (Just lat, Just lng)) -> return $ Just $ latLng lat lng
      _                           -> return Nothing
#endif

latLng :: Latitude -> Longitude -> LatLng
latLng lat lng = LatLng lat lng

lngLat :: Longitude -> Latitude -> LatLng
lngLat = flip latLng
