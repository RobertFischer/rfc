module RFC.Prelude
  ( module ClassyPrelude
  , module RFC.Prelude
  , module Data.UUID.Types
  , module Data.String.Conversions
  , module GHC.Generics
  , module Text.Read
  ) where

import Prelude ()
import ClassyPrelude hiding (Handler, unpack)
import Data.Char as Char
import Data.UUID.Types hiding (null, fromString)
import Data.String.Conversions (cs, LazyByteString, StrictByteString, LazyText, StrictText)
import GHC.Generics (Generic)
import Data.List as List
import Text.Read (Read, read)

charIsUpper :: Char -> Bool
charIsUpper = Char.isUpper

charIsLower :: Char -> Bool
charIsLower = Char.isLower

uniq :: (Eq a) => [a] -> [a]
uniq = List.nub
