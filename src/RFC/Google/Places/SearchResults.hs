module RFC.Google.Places.SearchResults
  ( module RFC.Google.Places.SearchResults
  ) where

import Data.Aeson ((.:), (.:?), withObject)
import RFC.Prelude
import RFC.JSON
import qualified Data.Maybe as Maybe
import RFC.Data.LatLng
import Data.Map as Map

type ResultsStatus = String
newtype Results = Results (ResultsStatus, [Result])

instance ToJSON Results where
  toJSON (Results (status, results)) =
    toJSON $ Map.fromList
      [ ("status"::String, toJSON status)
      , ("results"::String, toJSON results)
      ]

instance FromJSON Results where
  parseJSON = withObject "Places.Results" $ \topObj -> do
    status <- topObj .: "status"
    results <- topObj .:? "results"
    return $ Results (status, Maybe.fromMaybe [] results)

data Result = Result
  { resultLocation :: LatLng
  , resultName :: String
  , resultPlaceId :: String
  , resultVicinity :: Maybe String
  }

instance FromJSON Result where
  parseJSON = withObject "Places.Result" $ \topObj -> do
    geometry <- topObj .: "geometry"
    location <- geometry .: "location"
    lat <- location .: "lat"
    lng <- location .: "lng"
    let latLngLoc = latLng lat lng
    name <- topObj .: "name"
    id <- topObj .: "place_id"
    vicinity <- topObj .:? "vicinity"
    return $ Result latLngLoc name id vicinity

instance ToJSON Result where
  toJSON result = toJSON . Map.fromList $
    [ ("geometry"::String, toJSON . Map.fromList $
        [ ("location"::String, Map.fromList $
            [ ("lat"::String, latitude . resultLocation $ result)
            , ("lng"::String, longitude . resultLocation $ result)
            ]
          )
        ]
      )
    , ("name"::String, toJSON . resultName $ result)
    , ("place_id"::String, toJSON . resultPlaceId $ result)
    , ("vicinity"::String, toJSON . resultVicinity $ result)
    ]
