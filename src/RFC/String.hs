{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeSynonymInstances #-}

module RFC.String
  ( module RFC.String
  , module Data.String.Conversions
  , module Data.String.Conversions.Monomorphic
  ) where

import           Data.Function                       (($))
import           Data.String                         (String)
import           Data.String.Conversions             hiding ((<>))
import           Data.String.Conversions.Monomorphic hiding (fromString,
                                                      toString)
#ifndef GHCJS_BROWSER
import           Servant.Docs
#endif

type ConvertibleString = ConvertibleStrings -- I keep forgetting to pluralize this.
type ConvertibleToSBS a = ConvertibleStrings a StrictByteString
type ConvertibleFromSBS a = ConvertibleStrings StrictByteString a
type ConvertibleToString a = ConvertibleStrings a String
type ConvertibleFromString a = ConvertibleStrings String a

#ifndef GHCJS_BROWSER

instance ToSample StrictText where
  toSamples _ = singleSample $ cs "This is random text"

instance ToSample LazyText where
  toSamples _ = singleSample $ cs "This is random text"

#endif
