{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module RFC.Prelude
  ( module RFC.Prelude
  , module RFC.Data.UUID
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
  , module RFC.String
  , module Data.Word
  , module Data.Semigroup
  , module Control.Monad.Fail
  , module Data.Time.Clock
  , module UnliftIO
  , module GHC.Exts
  , module Control.Lens.Lens
  , module Control.Lens.Prism
  , module Control.Lens.Type
  , module Data.Proxy
  ) where

import           ClassyPrelude               hiding ( Day, fail, fromList, toList, unpack )
import           Control.Lens.Lens
import           Control.Lens.Prism
import           Control.Lens.Type
import           Control.Monad               ( forever, void, (<=<), (>=>) )
import           Control.Monad.Fail          ( MonadFail, fail )
import           Control.Monad.Trans.Control
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Char                   as Char
import           Data.Default
import qualified Data.Foldable               as Foldable
import           Data.Function               ( (&) )
import qualified Data.List                   as List
import           Data.Proxy                  ( Proxy (..) )
import           Data.Semigroup
import           Data.Time.Clock
import           Data.Time.Units
import           Data.Typeable               ( TypeRep, typeOf )
import           Data.Word                   ( Word16 )
import           GHC.Conc.Sync
import           GHC.Generics                ( Generic )
import           RFC.Data.UUID               ( UUID )
import           RFC.String
import           Text.Read                   ( Read, read )
import           UnliftIO
#ifdef VERSION_exceptions
import Control.Monad.Catch
#endif
import GHC.Exts ( IsList (..), fromListN )

ifThenElse :: Bool -> a -> a -> a
ifThenElse test true false =
  case test of
    True  -> true
    False -> false
{-# INLINE ifThenElse #-}

charIsUpper :: Char -> Bool
charIsUpper = Char.isUpper
{-# INLINE charIsUpper #-}

charIsLower :: Char -> Bool
charIsLower = Char.isLower
{-# INLINE charIsLower #-}

uniq :: (Eq a) => [a] -> [a]
uniq = List.nub
{-# INLINE uniq #-}

safeHead :: (MonadFail m, Foldable f) => f a -> m a
safeHead xs =
  case Foldable.toList xs of
    []    -> fail "Attempted to head an empty list"
    (x:_) -> return x
{-# INLINE safeHead #-}
{-# SPECIALIZE INLINE safeHead :: [a] -> Maybe a #-}
{-# SPECIALIZE INLINE safeHead :: [a] -> IO a #-}

foldl :: MonoFoldable mono => (a -> Element mono -> a) -> a -> mono -> a
foldl = foldl'
{-# INLINE foldl #-}

type Boolean = Bool -- I keep forgetting which Haskell uses....

newtype Failed = Failed String
  deriving (Show, Eq, Ord, Generic, Typeable)
instance Exception Failed

instance {-# OVERLAPPABLE #-} (Monad m, MonadIO m) => MonadFail m where
  fail = throwIO . Failed
  {-# INLINE fail #-}

#ifdef VERSION_exceptions
instance {-# OVERLAPPABLE #-} MonadThrow m => MonadFail m where
  fail = throwM . Failed
  {-# INLINE fail #-}
#endif

instance {-# OVERLAPPING #-} MonadFail STM where
  fail _ = retry
  {-# INLINE fail #-}

instance {-# OVERLAPPING #-} MonadFail (Either String) where
  fail = Left
  {-# INLINE fail #-}

instance {-# OVERLAPPING #-} MonadFail Option where
  fail _ = Option Nothing
  {-# INLINE fail #-}
