{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module RFC.GHCJS.JSString
	( JSString
	, toJSString
	, fromJSString
	, fromJSString'
	) where

import           Data.JSString  as JSString
import qualified Data.List      as List
import qualified Data.Text.Lazy as TL
import           RFC.Prelude

-- | Converts from any 'ToText' type to a 'JSString'.
toJSString :: (ToText str) => str -> JSString
toJSString = pack . toChars
{-# INLINE toJSString #-}

-- | Lazily converts from a JSString to any 'FromText' type.
fromJSString :: (FromText str) => JSString -> str
fromJSString = fromChars . unpack
{-# INLINE fromJSString #-}

-- | Strictly converts from a JSString to any 'FromText' type.
fromJSString' :: (FromText str) => JSString -> str
fromJSString' = fromChars . unpack'
{-# INLINE fromJSString' #-}

instance {-# OVERLAPPING #-} ToText JSString where
	toText = fromJSString'
	{-# INLINE toText #-}

instance {-# OVERLAPPING #-} FromText JSString where
	fromText = toJSString
	{-# INLINE fromText #-}

instance {-# OVERLAPPING #-} Semigroup JSString where
  (<>) = JSString.append

type instance Element JSString = Char

instance {-# OVERLAPPING #-} MonoFunctor JSString where
  omap :: (Char -> Char) -> JSString -> JSString
  omap f = toJSString . omap f . toLazyText
  {-# INLINE omap #-}

instance {-# OVERLAPPING #-} MonoFoldable JSString where
  ofoldMap f = ofoldr (mappend . f) mempty
  {-# INLINABLE ofoldMap #-}

  ofoldr :: (Char -> a -> a) -> a -> JSString -> a
  ofoldr f init str = TL.foldr f init $ toLazyText str
  {-# INLINE ofoldr #-}

  ofoldl' :: (a -> Char -> a) -> a -> JSString -> a
  ofoldl' f init str = TL.foldl' f init $ toLazyText str
  {-# INLINE ofoldl' #-}

  otoList = toChars
  {-# INLINE otoList #-}

  oall f str = TL.all f $ toLazyText str
  {-# INLINE oall #-}

  oany :: (Char -> Bool) -> JSString -> Bool
  oany f str = TL.any f $ toLazyText str
  {-# INLINE oany #-}

  onull = TL.null . toLazyText
  {-# INLINE onull #-}

  olength64 = TL.length . toLazyText
  {-# INLINE olength64 #-}

  ofoldr1Ex :: (Char -> Char -> Char) -> JSString -> Char
  ofoldr1Ex f str = doIt $ fromJSString str
    where
      doIt []           = '\0'
      doIt (start:rest) = List.foldr f start rest
  {-# INLINE ofoldr1Ex #-}

  ofoldl1Ex' :: (Char -> Char -> Char) -> JSString -> Char
  ofoldl1Ex' f str = doIt $ fromJSString str
    where
      doIt []           = '\0'
      doIt (start:rest) = List.foldl' f start rest
  {-# INLINE ofoldl1Ex' #-}

  headEx = TL.head . toLazyText
  {-# INLINE headEx #-}

  lastEx = TL.last . toLazyText
  {-# INLINE lastEx #-}
