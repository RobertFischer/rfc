module RFC.Json
( jsonOptions
, deriveJSON
, FromJSON
, ToJSON
) where

import RFC.Prelude
import Data.Aeson as JSON
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (Options(..), SumEncoding(..))

jsonOptions :: Options
jsonOptions = defaultOptions
    { sumEncoding = ObjectWithSingleField
    , unwrapUnaryRecords = True
    , fieldLabelModifier = flm
    }
  where
    flm [] = []
    flm (x:[]) = [x]
    flm (x:xs)
      | charIsLower x =
          case flm xs of
            [] -> (x:xs)
            result -> result
      | otherwise = (charToLower x):xs

