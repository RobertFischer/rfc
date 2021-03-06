{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}

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
  , module Data.Word
  , module Data.Semigroup
  , module Control.Monad.Fail
  , module Data.Time.Clock
  , module UnliftIO
  , module GHC.Exts
  , module Data.Proxy
  , module Data.Ratio
  , module Data.Tuple.Curry
  , module Data.Either
  , module Data.Int
  , module RFC.Prelude.Instances
  , module RFC.Prelude.JSON
  , module RFC.Prelude.String
  , module RFC.Prelude.Concurrent
  ) where

import           ClassyPrelude                hiding
  ( Day, fail, fromList, map, pack, readMay, toList, unpack, (++) )
import           Control.Monad                ( forever, void, (<=<), (>=>) )
import           Control.Monad.Fail           ( MonadFail, fail )
import           Control.Monad.Trans.Control
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Char                    as Char
import           Data.Default
import           Data.Either                  ( Either (..) )
import qualified Data.Foldable                as Foldable
import           Data.Function                ( (&) )
import           Data.Int
import qualified Data.List                    as List
import           Data.Proxy                   ( Proxy (..) )
import           Data.Ratio                   ( Ratio, Rational )
import           Data.Semigroup
import           Data.Time.Clock
import           Data.Time.Units
import           Data.Tuple.Curry             ( curryN, uncurryN )
import           Data.Typeable                ( TypeRep, typeOf )
import           Data.Word                    ( Word16 )
import           GHC.Exts                     ( IsList (..), fromListN )
import           GHC.Generics                 ( Generic )
import           Math.NumberTheory.Logarithms ( integerLog2 )
import           RFC.Data.UUID                ( UUID )
import           RFC.Prelude.Concurrent
import           RFC.Prelude.Instances
import           RFC.Prelude.JSON
import           RFC.Prelude.String
import           Text.Read                    ( Read, read, readMaybe )
import           UnliftIO

#ifdef VERSION_exceptions
import           Control.Monad.Catch
#endif

{-# ANN module "HLint: ignore Use if" #-}

readMay :: (ToText input, Read output) => input -> Maybe output
readMay = readMaybe . fromText . toText
{-# INLINEABLE readMay #-}
{-# SPECIALIZE INLINE readMay :: (Read output) => StrictText -> Maybe output #-}
{-# SPECIALIZE INLINE readMay :: (Read output) => String -> Maybe output #-}

-- | Unwraps an 'Either', returning a default value if it is a 'Left'.
fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b (Left  _) = b
{-# INLINE fromRight #-}

-- | Unwraps an 'Either', returning a default value if it is a 'Right'.
fromLeft :: a -> Either a b -> a
fromLeft a (Right _) = a
fromLeft _ (Left  a) = a
{-# INLINE fromLeft #-}

-- | An equivalent to 'maybe' for 'Right'.
right :: x -> (b -> x) -> Either a b -> x
right _ f (Right b) = f b
right x _ (Left _)  = x
{-# INLINE right #-}
{-# SPECIALIZE INLINE right :: x -> (String -> x) -> Either a String -> x #-}

-- | An equivalent to 'maybe' for 'Left'.
left :: x -> (a -> x) -> Either a b -> x
left _ f (Left a)  = f a
left x _ (Right _) = x
{-# INLINE left #-}
{-# SPECIALIZE INLINE left :: x -> (String -> x) -> Either String b -> x #-}

-- | Our implementation of if, which is pretty much exactly what you would suspect.
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
{-# INLINEABLE safeHead #-}
{-# SPECIALIZE INLINE safeHead :: [a] -> Maybe a #-}
{-# SPECIALIZE INLINE safeHead :: [a] -> IO a #-}

lg :: Integer -> Integer
lg z
  | z > 0 = toInteger $ integerLog2 z
  | z == 0 = 0
  | otherwise = toInteger $ integerLog2 (-1 * z)
{-# INLINEABLE lg #-}

lg' :: (Integral z, Integral z') => z -> z'
lg' = fromInteger . lg . toInteger
{-# INLINEABLE lg' #-}
{-# SPECIALIZE INLINE lg' :: Integer -> Integer             #-}
{-# SPECIALIZE INLINE lg' :: (Integral z') => Integer -> z' #-}
{-# SPECIALIZE INLINE lg' :: (Integral z ) => z -> Int      #-}
{-# SPECIALIZE INLINE lg' :: (Integral z ) => z -> Integer  #-}
{-# SPECIALIZE INLINE lg' :: Word -> Word                   #-}

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
{-# SPECIALIZE INLINE (<$$>) :: Functor g => (a->b) -> [g a] -> [g b]  #-}
{-# SPECIALIZE INLINE (<$$>) :: Functor f => (a->b) -> f [a] -> f [b]  #-}
{-# SPECIALIZE INLINE (<$$>) :: Functor g => (a->b) -> IO (g a) -> IO (g b) #-}
{-# SPECIALIZE INLINE (<$$>) :: Functor f => (a->b) -> f (IO a) -> f (IO b) #-}
{-# SPECIALIZE INLINE (<$$>) :: Functor g => (a->b) -> Maybe (g a) -> Maybe (g b) #-}
{-# SPECIALIZE INLINE (<$$>) :: Functor f => (a->b) -> f (Maybe a) -> f (Maybe b) #-}
