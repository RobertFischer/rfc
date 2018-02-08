{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}


module RFC.Miso.String
  ( module RFC.Miso.String
  ) where

import           Data.Function           (id, (.))
import           Data.String             (String)
import           Data.String.Conversions
import           Miso.String             (MisoString, ToMisoString (..))
import           RFC.Data.UUID           as UUID

instance {-# OVERLAPPING #-} ConvertibleStrings UUID.UUID MisoString where
  convertString = toMisoString . UUID.toString

instance {-# OVERLAPPING #-} ConvertibleStrings MisoString MisoString where
  convertString = id

instance {-# OVERLAPPABLE #-} (ToMisoString str) => ConvertibleStrings MisoString str where
  {-# SPECIALIZE instance ConvertibleStrings MisoString LazyText         #-}
  {-# SPECIALIZE instance ConvertibleStrings MisoString StrictText       #-}
  {-# SPECIALIZE instance ConvertibleStrings MisoString LazyByteString   #-}
  {-# SPECIALIZE instance ConvertibleStrings MisoString StrictByteString #-}
  {-# SPECIALIZE instance ConvertibleStrings MisoString String           #-}
  convertString = fromMisoString

instance {-# OVERLAPPABLE #-} (ToMisoString str) => ConvertibleStrings str MisoString where
  {-# SPECIALIZE instance ConvertibleStrings LazyText MisoString         #-}
  {-# SPECIALIZE instance ConvertibleStrings StrictText MisoString       #-}
  {-# SPECIALIZE instance ConvertibleStrings LazyByteString MisoString   #-}
  {-# SPECIALIZE instance ConvertibleStrings StrictByteString MisoString #-}
  {-# SPECIALIZE instance ConvertibleStrings String MisoString           #-}
  convertString = toMisoString

