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

import           Data.MonoTraversable
import           Data.String                         (String)
import           Data.String.Conversions
import           Data.String.Conversions.Monomorphic (toLazyText, toString)
import qualified Data.Text.Lazy                      as TL
import           Miso.String                         (MisoString,
                                                      ToMisoString (..))
import           Miso.Subscription.History           (URI (..))
import qualified RFC.Data.UUID                       as UUID
import           RFC.Prelude

type instance Element MisoString = Char

instance MonoFunctor MisoString where
  omap :: (Char -> Char) -> MisoString -> MisoString
  omap f = toMisoString . omap f . toLazyText
  {-# INLINE omap #-}

instance MonoFoldable MisoString where
  ofoldMap f = ofoldr (mappend . f) mempty
  {-# INLINE ofoldMap #-}

  ofoldr :: (Char -> a -> a) -> a -> MisoString -> a
  ofoldr f init str = TL.foldr f init $ toLazyText str
  {-# INLINE ofoldr #-}

  ofoldl' :: (a -> Char -> a) -> a -> MisoString -> a
  ofoldl' f init str = TL.foldl' f init $ toLazyText str
  {-# INLINE ofoldl' #-}

  otoList = toString
  {-# INLINE otoList #-}

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
  ofoldr1Ex f str = TL.foldr1 f $ toLazyText str
  {-# INLINE ofoldr1Ex #-}

  ofoldl1Ex' :: (Char -> Char -> Char) -> MisoString -> Char
  ofoldl1Ex' f str = TL.foldl1' f $ toLazyText str
  {-# INLINE ofoldl1Ex' #-}

  headEx = TL.head . toLazyText
  {-# INLINE headEx #-}

  lastEx = TL.last . toLazyText
  {-# INLINE lastEx #-}

instance {-# OVERLAPPING #-} ConvertibleStrings URI MisoString where
  convertString = toMisoString . show
  {-# INLINE convertString #-}

instance {-# OVERLAPPING #-} ConvertibleStrings URI URI where
  convertString = id
  {-# INLINE convertString #-}

instance {-# OVERLAPPABLE #-} (ConvertibleStrings String a) => ConvertibleStrings URI a where
  {-# SPECIALIZE instance ConvertibleStrings URI String           #-}
  {-# SPECIALIZE instance ConvertibleStrings URI LazyText         #-}
  {-# SPECIALIZE instance ConvertibleStrings URI StrictText       #-}
  {-# SPECIALIZE instance ConvertibleStrings URI LazyByteString   #-}
  {-# SPECIALIZE instance ConvertibleStrings URI StrictByteString #-}
  convertString = convertString . show
  {-# INLINE convertString #-}

instance {-# OVERLAPPING #-} ConvertibleStrings UUID.UUID MisoString where
  convertString = toMisoString . UUID.toString
  {-# INLINE convertString #-}

instance {-# OVERLAPPING #-} ConvertibleStrings MisoString MisoString where
  convertString = id
  {-# INLINE convertString #-}

instance {-# OVERLAPPABLE #-} (ToMisoString str) => ConvertibleStrings MisoString str where
  {-# SPECIALIZE instance ConvertibleStrings MisoString String           #-}
  {-# SPECIALIZE instance ConvertibleStrings MisoString LazyText         #-}
  {-# SPECIALIZE instance ConvertibleStrings MisoString StrictText       #-}
  {-# SPECIALIZE instance ConvertibleStrings MisoString LazyByteString   #-}
  {-# SPECIALIZE instance ConvertibleStrings MisoString StrictByteString #-}
  convertString = fromMisoString
  {-# INLINE convertString #-}

instance {-# OVERLAPPABLE #-} (ToMisoString str) => ConvertibleStrings str MisoString where
  {-# SPECIALIZE instance ConvertibleStrings String MisoString           #-}
  {-# SPECIALIZE instance ConvertibleStrings LazyText MisoString         #-}
  {-# SPECIALIZE instance ConvertibleStrings StrictText MisoString       #-}
  {-# SPECIALIZE instance ConvertibleStrings LazyByteString MisoString   #-}
  {-# SPECIALIZE instance ConvertibleStrings StrictByteString MisoString #-}
  convertString = toMisoString
  {-# INLINE convertString #-}

