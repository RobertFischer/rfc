{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module RFC.Prelude.Concurrent
  ( module RFC.Prelude.Concurrent
  , module Control.Monad.IO.Unlift
  , module UnliftIO.Concurrent
  , module UnliftIO.Async
  , module Control.Monad.Trans.Control
  ) where

import           ClassyPrelude
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Control
import           Data.Foldable               ( foldrM )
import           UnliftIO.Async
import           UnliftIO.Concurrent

{-# ANN module "HLint: ignore Use if" #-}

-- |Executes all the IO actions simultaneously and returns the original data structure with the arguments replaced
--  by the results of the execution.
doConcurrently :: (Traversable t, MonadUnliftIO m) => t (m a) -> m (t a)
doConcurrently = mapConcurrently id
{-# SPECIALIZE INLINE doConcurrently :: (MonadUnliftIO m) => [m a] -> m [a] #-}
{-# SPECIALIZE INLINE doConcurrently :: [IO a] -> IO [a]                    #-}
{-# INLINE doConcurrently #-}

-- |Executes all the IO actions simulataneously and discards the results.
doConcurrently_ :: (Foldable f, MonadUnliftIO m) => f (m a) -> m ()
doConcurrently_ = mapConcurrently_ id
{-# SPECIALIZE INLINE doConcurrently_ :: (MonadUnliftIO m) => [m a] -> m () #-}
{-# SPECIALIZE INLINE doConcurrently_ :: [IO a] -> IO ()                    #-}
{-# INLINE doConcurrently_ #-}

-- |Executes all the IO actions simultaneously and then filters the results based on the filter function.
filterConcurrently :: (Traversable t, Applicative t, Semigroup (t a), Monoid (t a), MonadUnliftIO m) => (a -> Bool) -> t (m a) -> m (t a)
filterConcurrently test actions = do
    !asyncActions <- mapM async actions
    foldrM foldImpl mempty asyncActions
  where
    foldImpl !promise results = do
      result <- wait promise
      return $ case test result of
        True -> (pure result) <> results
        False -> results
{-# INLINABLE filterConcurrently #-}
{-# SPECIALIZE INLINE filterConcurrently :: (MonadUnliftIO m) => (a -> Bool) -> [m a] -> m [a] #-}
{-# SPECIALIZE INLINE filterConcurrently :: (a -> Bool) -> [IO a] -> IO [a]                    #-}
