module RFC.Prelude
  ( module RFC.Prelude
  , module RFC.Data.UUID
  , module Data.String.Conversions
  , module GHC.Generics
  , module Text.Read
  , module Data.Time.Units
  , module Data.Function
  , module Data.Typeable
  , module Control.Monad
  , module Data.Bitraversable
  , module Data.Bifunctor
  , module Data.Bifoldable
  , module Data.Default
  , module ClassyPrelude
  , module Control.Monad.Trans.Control
  , module UnliftIO.Exception
  , module Control.Monad.IO.Unlift
  , module RFC.String
  , module Data.Word
  ) where

import           ClassyPrelude               hiding (Day, unpack)
import           Control.Monad               (forever, void, (<=<), (>=>))
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Control
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Char                   as Char
import           Data.Default
import           Data.Function               ((&))
import qualified Data.List                   as List
import           Data.String.Conversions     (LazyByteString, LazyText,
                                              StrictByteString, StrictText, cs)
import           Data.Time.Units
import           Data.Typeable               (TypeRep, typeOf)
import           Data.Word                   (Word16)
import           GHC.Generics                (Generic)
import           RFC.Data.UUID               (UUID)
import           RFC.String
import           Text.Read                   (Read, read)
import           UnliftIO.Exception

charIsUpper :: Char -> Bool
charIsUpper = Char.isUpper

charIsLower :: Char -> Bool
charIsLower = Char.isLower

uniq :: (Eq a) => [a] -> [a]
uniq = List.nub

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x


type Boolean = Bool -- I keep forgetting which Haskell uses....
