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

type Latitude = Double
type Longitude = Double


data LatLng = LatLng {
  latitude  :: Latitude,
  longitude :: Longitude
} deriving (Eq, Ord, Show, Typeable, Generic)
$(JSON.deriveJSON JSON.jsonOptions ''LatLng)

latLng :: Latitude -> Longitude -> LatLng
latLng = LatLng

lngLat :: Longitude -> Latitude -> LatLng
lngLat = flip latLng
