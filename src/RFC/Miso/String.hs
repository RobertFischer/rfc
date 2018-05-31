{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}


module RFC.Miso.String
  ( module RFC.Miso.String
  , module Miso.String
  ) where

import           Miso.String          (MisoString, ToMisoString (..))
import           RFC.Data.UUID        ()
import           RFC.String           ()

#ifdef GHCJS
import qualified Data.JSString        as JSString
import qualified Data.List            as List
import           Data.MonoTraversable
import qualified Data.Text.Lazy       as TL
import           RFC.Prelude

instance Semigroup MisoString where
  (<>) = JSString.append

type instance Element MisoString = Char

instance MonoFunctor MisoString where
  omap :: (Char -> Char) -> MisoString -> MisoString
  omap f = toMisoString . omap f . toLazyText
  {-# INLINE omap #-}

instance MonoFoldable MisoString where
  ofoldMap f = ofoldr (mappend . f) mempty
  {-# INLINABLE ofoldMap #-}

  ofoldr :: (Char -> a -> a) -> a -> MisoString -> a
  ofoldr f init str = TL.foldr f init $ toLazyText str
  {-# INLINE ofoldr #-}

  ofoldl' :: (a -> Char -> a) -> a -> MisoString -> a
  ofoldl' f init str = TL.foldl' f init $ toLazyText str
  {-# INLINE ofoldl' #-}

  otoList = ofoldr (:) []
  {-# INLINABLE otoList #-}

  oall f str = TL.all f $ toLazyText str
  {-# INLINE oall #-}

  oany :: (Char -> Bool) -> MisoString -> Bool
  oany f str = TL.any f $ toLazyText str
  {-# INLINE oany #-}

  onull = TL.null . toLazyText
  {-# INLINE onull #-}

  olength64 = TL.length . toLazyText
  {-# INLINE olength64 #-}

  ofoldr1Ex :: (Char -> Char -> Char) -> MisoString -> Char
  ofoldr1Ex f str = doIt $ fromMisoString str
    where
      doIt []           = '\0'
      doIt (start:rest) = List.foldr f start rest
  {-# INLINE ofoldr1Ex #-}

  ofoldl1Ex' :: (Char -> Char -> Char) -> MisoString -> Char
  ofoldl1Ex' f str = doIt $ fromMisoString str
    where
      doIt []           = '\0'
      doIt (start:rest) = List.foldl' f start rest
  {-# INLINE ofoldl1Ex' #-}

  headEx = TL.head . toLazyText
  {-# INLINE headEx #-}

  lastEx = TL.last . toLazyText
  {-# INLINE lastEx #-}

instance {-# OVERLAPPING #-} FromText MisoString where
  fromText :: Text -> MisoString
  fromText = toMisoString
  {-# INLINE fromText #-}

instance {-# OVERLAPPING #-} ToText MisoString where
  toText :: MisoString -> Text
  toText = fromMisoString
  {-# INLINE toText #-}

#endif
