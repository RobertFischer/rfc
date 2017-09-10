module RFC.Concurrent
  ( module RFC.Concurrent
  , module Control.Concurrent.Async
  ) where

import RFC.Prelude hiding (mapConcurrently)
import Control.Concurrent.Async (mapConcurrently, mapConcurrently_)

-- |Executes all the IO actions simultaneously and returns the original data structure with the arguments replaced
--  by the results of the execution.
doConcurrently :: (Traversable t) => t (IO a) -> IO (t a)
doConcurrently = mapConcurrently id

-- |Executes all the IO actions simulataneously and discards the results.
doConcurrently_ :: (Foldable f) => f (IO a) -> IO ()
doConcurrently_ = mapConcurrently_ id

-- |Executes all the IO actions simultaneously, feeds them into the filter function, and then
-- filters the results.
filterConcurrently :: (a -> Bool) -> [IO a] -> IO [a] -- TODO Seems like we should be able to do this with Foldable/Traversable
filterConcurrently filterFunc actions =
    filter filterFunc <$> doConcurrently actions
