{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports  #-}

module RFC.Miso
  ( module RFC.Miso.String
#ifdef VERSION_rfc_ghcjs
  , module RFC.GHCJS
#endif
  ) where

import           RFC.Miso.String
#ifdef VERSION_rfc_ghcjs
import           RFC.GHCJS
#endif
