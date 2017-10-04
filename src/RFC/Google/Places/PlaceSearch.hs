module RFC.Google.Places.PlaceSearch
  ( module RFC.Google.Places.PlaceSearch
  , module RFC.Data.LatLng
  , HasAPIClient
  ) where

import Data.Aeson ((.:), (.:?), withObject)
import RFC.Prelude
import RFC.JSON
import RFC.HTTP.Client
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import RFC.Data.LatLng

endpoint :: URL
endpoint =
  case importURL endpointStr of
    Nothing -> error $ "Could not parse the Google Place Search API endpoint into a URL: " ++ endpointStr
    (Just it) -> it
  where
    endpointStr = "https://maps.googleapis.com/maps/api/place/textsearch/json"

data Params = Params
  { apiKey :: String
  , search :: String
  , language :: Maybe String
  , placeType :: Maybe PlaceType -- ^ "type"
  }

data PlaceType =
  Hospital | Doctor

placeTypeToString :: PlaceType -> String
placeTypeToString Hospital = "hospital"
placeTypeToString Doctor = "doctor"

paramsToPairs :: Params -> [(String,String)]
paramsToPairs params =
    [ ("key", apiKey params)
    , ("query", search params)
    ]  ++ optionalPairs
  where
    toPair = optionalParamToPair params
    optionalPairs = Maybe.catMaybes
      [ toPair language "language"
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
  parseJSON = withObject "PlacesSearch.Results" $ \topObj -> do
    status <- topObj .: "status"
    results <- topObj .:? "results"
    return $ Results (status, Maybe.fromMaybe [] results)

data Result = Result
  { resultLocation :: LatLng
  , resultName :: String
  , resultPlaceId :: String
  }

instance FromJSON Result where
  parseJSON = withObject "PlacesSearch.Result" $ \topObj -> do
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
