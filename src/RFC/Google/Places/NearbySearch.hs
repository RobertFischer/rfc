module RFC.Google.Places.NearbySearch
  ( module RFC.Google.Places.NearbySearch
  , HasAPIClient
  ) where

import Data.Aeson ((.:), (.:?), withObject)
import RFC.Prelude
import RFC.JSON
import RFC.HTTP.Client
import qualified Data.Maybe as Maybe
import qualified Data.List as List

endpoint :: URL
endpoint =
  case importURL endpointStr of
    Nothing -> error $ "Could not parse the Google Places API endpoint into a URL: " ++ endpointStr
    (Just it) -> it
  where
    endpointStr = "https://maps.googleapis.com/maps/api/place/nearbysearch/json"

type Latitude = Double
type Longitude = Double

newtype LatLng = LatLng (Latitude,Longitude)

latLng :: Latitude -> Longitude -> LatLng
latLng lat lng = LatLng (lat,lng)

lngLat :: Longitude -> Latitude -> LatLng
lngLat = flip latLng

longitude :: LatLng -> Longitude
longitude (LatLng(_,l)) = l

latitude :: LatLng -> Latitude
latitude (LatLng(l,_)) = l

data Params = Params
  { apiKey :: String
  , location :: LatLng
  , radiusMeters :: Integer -- ^ maximum of 50,000
  , rankBy :: RankBy
  , keyword :: Maybe String
  , language :: Maybe String
  , region :: Maybe String
  , placeType :: Maybe PlaceType -- ^ "type"
  }

data RankBy = Distance | Prominence

rankByToString :: RankBy -> String
rankByToString Distance = "distance"
rankByToString Prominence = "prominence"

data OptionalParams = OptionalParams

data PlaceType =
  Hospital | Doctor

placeTypeToString :: PlaceType -> String
placeTypeToString Hospital = "hospital"
placeTypeToString Doctor = "doctor"

paramsToPairs :: Params -> [(String,String)]
paramsToPairs params =
    [ ("key", apiKey params)
    , ("location", (show lat) ++ "," ++ (show lng))
    , ("radius", show $ radiusMeters params)
    , ("rankBy", rankByToString $ rankBy params)
    ] ++ optionalPairs
  where
    loc = location params
    (lat,lng) = (latitude loc, longitude loc)
    toPair = optionalParamToPair params
    optionalPairs = Maybe.catMaybes
      [ toPair keyword "keyword"
      , toPair language "language"
      , toPair region "region"
      , toPair (fmap placeTypeToString . placeType) "type"
      ]

optionalParamToPair :: Params -> (Params -> Maybe String) -> String -> Maybe (String,String)
optionalParamToPair param field paramName = (\paramVal -> (paramName, paramVal)) <$> field param

paramsToUrl :: Params -> URL
paramsToUrl params =
    List.foldr fold endpoint $ paramsToPairs params
  where
    fold = flip add_param

type ResultsStatus = String
newtype Results = Results (ResultsStatus, [Result])

instance FromJSON Results where
  parseJSON = withObject "NearbySearch.Results" $ \topObj -> do
    status <- topObj .: "status"
    results <- topObj .:? "results"
    return $ Results (status, Maybe.fromMaybe [] results)

data Result = Result
  { resultLocation :: LatLng
  , resultName :: String
  , resultPlaceId :: String
  }

instance FromJSON Result where
  parseJSON = withObject "NearbySearch.Result" $ \topObj -> do
    geometry <- topObj .: "geometry"
    location <- geometry .: "location"
    lat <- location .: "lat"
    lng <- location .: "lng"
    let latLngLoc = latLng lat lng
    name <- topObj .: "name"
    id <- topObj .: "place_id"
    return $ Result latLngLoc name id

query :: (HasAPIClient m) => Params -> m Results
query = apiGet . paramsToUrl
