{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module RFC.Concurrent
  ( module RFC.Concurrent
  , module Control.Concurrent.Async.Lifted
  ) where

import           Control.Concurrent.Async.Lifted
import           RFC.Prelude                     hiding (mapConcurrently)

-- |Executes all the IO actions simultaneously and returns the original data structure with the arguments replaced
--  by the results of the execution.
doConcurrently :: (Traversable t, MonadBaseControl IO m) => t (m a) -> m (t a)
doConcurrently = mapConcurrently id

-- |Executes all the IO actions simulataneously and discards the results.
doConcurrently_ :: (Foldable f, MonadBaseControl IO m) => f (m a) -> m ()
doConcurrently_ = mapConcurrently_ id

-- |Executes all the IO actions simultaneously, feeds them into the filter function, and then
-- filters the results.
filterConcurrently :: (MonadBaseControl IO m) => (a -> Bool) -> [m a] -> m [a] -- TODO Seems like we should be able to do this with Foldable/Traversable
filterConcurrently filterFunc actions =
    filter filterFunc <$> doConcurrently actions
