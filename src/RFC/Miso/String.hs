{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}


module RFC.Miso.String
  ( module RFC.Miso.String
  , module Miso.String
  ) where

import           Data.Function             (id, (.))
import           Data.String               (String)
import           Data.String.Conversions
import           Miso.String               (MisoString, ToMisoString (..))
import           Miso.Subscription.History (URI (..))
import           RFC.Data.UUID             as UUID
import           Text.Show


instance {-# OVERLAPPING #-} ConvertibleStrings URI MisoString where
  convertString = toMisoString . show

instance {-# OVERLAPPING #-} ConvertibleStrings URI URI where
  convertString = id

instance {-# OVERLAPPABLE #-} (ConvertibleStrings String a) => ConvertibleStrings URI a where
  {-# SPECIALIZE instance ConvertibleStrings URI String           #-}
  {-# SPECIALIZE instance ConvertibleStrings URI LazyText         #-}
  {-# SPECIALIZE instance ConvertibleStrings URI StrictText       #-}
  {-# SPECIALIZE instance ConvertibleStrings URI LazyByteString   #-}
  {-# SPECIALIZE instance ConvertibleStrings URI StrictByteString #-}
  convertString = convertString . show

instance {-# OVERLAPPING #-} ConvertibleStrings UUID.UUID MisoString where
  convertString = toMisoString . UUID.toString

instance {-# OVERLAPPING #-} ConvertibleStrings MisoString MisoString where
  convertString = id

instance {-# OVERLAPPABLE #-} (ToMisoString str) => ConvertibleStrings MisoString str where
  {-# SPECIALIZE instance ConvertibleStrings MisoString String           #-}
  {-# SPECIALIZE instance ConvertibleStrings MisoString LazyText         #-}
  {-# SPECIALIZE instance ConvertibleStrings MisoString StrictText       #-}
  {-# SPECIALIZE instance ConvertibleStrings MisoString LazyByteString   #-}
  {-# SPECIALIZE instance ConvertibleStrings MisoString StrictByteString #-}
  convertString = fromMisoString

instance {-# OVERLAPPABLE #-} (ToMisoString str) => ConvertibleStrings str MisoString where
  {-# SPECIALIZE instance ConvertibleStrings String MisoString           #-}
  {-# SPECIALIZE instance ConvertibleStrings LazyText MisoString         #-}
  {-# SPECIALIZE instance ConvertibleStrings StrictText MisoString       #-}
  {-# SPECIALIZE instance ConvertibleStrings LazyByteString MisoString   #-}
  {-# SPECIALIZE instance ConvertibleStrings StrictByteString MisoString #-}
  convertString = toMisoString

