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
  , module Control.Monad
  , module Data.Bitraversable
  , module Data.Bifunctor
  , module Data.Bifoldable
  , module Data.Default
  , module Control.Monad.Trans.Control
  , module Control.Monad.Catch
  ) where

import           ClassyPrelude               hiding (Day, Handler, bracket,
                                              bracketOnError, bracket_, catch,
                                              catchJust, catches, finally,
                                              handle, handleJust, mask, mask_,
                                              onException, try, tryJust,
                                              uninterruptibleMask,
                                              uninterruptibleMask_, unpack)
import           Control.Monad               (forever, void, (<=<), (>=>))
import           Control.Monad.Catch
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
import           GHC.Generics                (Generic)
import           Prelude                     ()
import           RFC.Data.UUID               (UUID)
import           Text.Read                   (Read, read)

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
safeHead []    = Nothing
safeHead (x:_) = Just x

throw :: (MonadThrow m, Exception e) => e -> m a
throw = throwM

type Boolean = Bool -- I keep forgetting which Haskell uses....
