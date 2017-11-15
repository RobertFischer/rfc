module RFC.Prelude
  ( module ClassyPrelude
  , module RFC.Prelude
  , module RFC.Data.UUID
  , module Data.String.Conversions
  , module GHC.Generics
  , module Text.Read
  , module Data.Time.Units
  , module Data.Function
  , module Data.Typeable
  ) where

import Prelude ()
import ClassyPrelude hiding (Handler, unpack, Day)
import Data.Char as Char
import Data.String.Conversions (cs, LazyByteString, StrictByteString, LazyText, StrictText)
import GHC.Generics (Generic)
import qualified Data.List as List
import Text.Read (Read, read)
import Data.Time.Units
import RFC.Data.UUID (UUID)
import Data.Function ((&))
import Data.Typeable (TypeRep, typeOf)

charIsUpper :: Char -> Bool
charIsUpper = Char.isUpper

charIsLower :: Char -> Bool
charIsLower = Char.isLower

uniq :: (Eq a) => [a] -> [a]
uniq = List.nub

mapFst :: (a -> c) -> (a,b) -> (c,b)
mapFst f (a,b) = (f a, b)

mapSnd :: (b -> c) -> (a,b) -> (a,c)
mapSnd f (a,b) = (a, f b)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x


