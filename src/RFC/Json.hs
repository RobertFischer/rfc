module RFC.Json
( jsonOptions
, deriveJSON
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
    flm (c:[]) = [c]
    flm (c:cs)
      | isLower c =
        case flm cs of
          [] -> (c:cs)
          result -> result
      | otherwise = (charToLower c):cs

