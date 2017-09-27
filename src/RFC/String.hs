module RFC.String
  ( module RFC.String
  , module Data.String.Conversions
  , module Data.String.Conversions.Monomorphic
  ) where

import Data.String.Conversions hiding ((<>))
import Data.String.Conversions.Monomorphic hiding (fromString, toString)

type ConvertibleString = ConvertibleStrings -- I keep forgetting to pluralize this.
type ConvertibleToSBS a = ConvertibleStrings a StrictByteString
type ConvertibleFromSBS a = ConvertibleStrings StrictByteString a


