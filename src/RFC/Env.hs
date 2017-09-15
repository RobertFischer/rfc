module RFC.Env
  ( checkDevelopment
  ) where

import RFC.Prelude
import Data.Maybe (maybe)
import System.Environment (lookupEnv)

checkDevelopment :: IO Bool
checkDevelopment = maybe True ((/=) "development") <$> lookupEnv "ENV"
