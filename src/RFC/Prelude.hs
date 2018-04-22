{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

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
  , module Data.Ratio
  , module RFC.Prelude.Instances
  , module Data.Tuple.Curry
  ) where

import           ClassyPrelude               hiding
  ( Day, fail, fromList, map, toList, unpack, (++) )
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
import           GHC.Generics                ( Generic )
import           RFC.Data.UUID               ( UUID )
import           RFC.String
import           Text.Read                   ( Read, read )
import           UnliftIO
#ifdef VERSION_exceptions
import           Control.Monad.Catch
#endif
import           Data.Ratio                  ( Ratio, Rational )
import           Data.Tuple.Curry            ( curryN, uncurryN )
import           GHC.Exts                    ( IsList (..), fromListN )
import           RFC.Prelude.Instances

{-# ANN module "HLint: ignore Use if" #-}

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

lg :: Integer -> Integer
lg 0 = 0
lg z = 1 + lg (z `div` 2)
{-# INLINEABLE lg #-}

lg' :: (Integral z, Integral z') => z -> z'
lg' z = fromInteger $
  if z > -1 && z < 1 then
    0
  else
    1 + lg ((toInteger z) `div` 2)
{-# INLINEABLE lg' #-}
{-# SPECIALIZE INLINE lg' :: Integer -> Integer #-}
{-# SPECIALIZE INLINE lg' :: Int -> Int         #-}
{-# SPECIALIZE INLINE lg' :: Integer -> Int     #-}
{-# SPECIALIZE INLINE lg' :: Int -> Integer     #-}

isSingleton :: (Foldable f) => f a -> Bool
isSingleton xs =
  case Foldable.toList xs of
    [_] -> True
    _   -> False
{-# INLINEABLE isSingleton #-}
{-# SPECIALIZE INLINE isSingleton :: [a] -> Bool   #-}
{-# SPECIALIZE INLINE isSingleton :: Seq a -> Bool #-}

fmap2 :: (Functor f, Functor g) => (a -> b) -> f(g a) -> f(g b)
fmap2 f x = fmap f <$> x
{-# INLINE fmap2 #-}
{-# SPECIALIZE INLINE fmap2 :: Functor g => (a->b) -> [g a] -> [g b]  #-}
{-# SPECIALIZE INLINE fmap2 :: Functor f => (a->b) -> f [a] -> f [b]  #-}
{-# SPECIALIZE INLINE fmap2 :: Functor g => (a->b) -> IO (g a) -> IO (g b) #-}
{-# SPECIALIZE INLINE fmap2 :: Functor f => (a->b) -> f (IO a) -> f (IO b) #-}
{-# SPECIALIZE INLINE fmap2 :: Functor g => (a->b) -> Maybe (g a) -> Maybe (g b) #-}
{-# SPECIALIZE INLINE fmap2 :: Functor f => (a->b) -> f (Maybe a) -> f (Maybe b) #-}


infixl 4 <$$>
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f(g a) -> f(g b)
(<$$>) = fmap2
{-# INLINE (<$$>) #-}
