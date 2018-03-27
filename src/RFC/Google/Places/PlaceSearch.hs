{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module RFC.Google.Places.PlaceSearch
  ( module RFC.Google.Places.PlaceSearch
  , module RFC.Google.Places.SearchResults
  , module RFC.Data.LatLng
  , HasAPIClient
  ) where

import qualified Data.List                       as List
import qualified Data.Maybe                      as Maybe
import           RFC.Data.LatLng
import           RFC.Google.Places.SearchResults
import           RFC.HTTP.Client
import           RFC.Log
import           RFC.Prelude

endpoint :: URL
endpoint =
  case importURL endpointStr of
    Nothing -> error $ "Could not parse the Google Place Search API endpoint into a URL: " <> endpointStr
    (Just it) -> it
  where
    endpointStr = "https://maps.googleapis.com/maps/api/place/textsearch/json"

data Params = Params
  { apiKey    :: String
  , search    :: String
  , language  :: Maybe String
  , placeType :: Maybe PlaceType -- ^ "type"
  }

data PlaceType =
  Hospital | Doctor deriving (Show,Eq,Ord,Enum,Bounded,Generic,Typeable)

placeTypeToString :: PlaceType -> String
placeTypeToString Hospital = "hospital"
placeTypeToString Doctor   = "doctor"

paramsToPairs :: Params -> [(String,String)]
paramsToPairs params =
    [ ("key", apiKey params)
    , ("query", search params)
    ]  <> optionalPairs
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

query :: (MonadIO m, MonadCatch m, HasAPIClient m) => Params -> m Results
query params = apiGet (paramsToUrl params) onError
  where
    onError :: (MonadIO m) => SomeException -> m Results
    onError err = do
      logWarn . cs $ "Error performing Google Place Search: " <> (show err)
      return $ Results (show err, [])
