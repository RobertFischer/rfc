{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module RFC.Prelude.Instances
  ( module RFC.Prelude.Instances
  ) where

import           ClassyPrelude
import           Control.Monad.Fail
import           Data.Semigroup
import           GHC.Conc

#ifdef VERSION_data_default
import           Data.Default
#endif

#ifdef VERSION_exceptions
import           Control.Monad.Catch
#endif

#ifdef VERSION_mtl
import           Control.Monad.Except ( ExceptT (..) )
#endif

{-# ANN module "HLint: ignore Unused LANGUAGE pragma" #-}

type Boolean = Bool -- I keep forgetting which Haskell uses....

newtype Failed = Failed String
  deriving (Show, Eq, Ord, Generic, Typeable)
instance Exception Failed

instance {-# OVERLAPPABLE #-} (Monad m, MonadIO m) => MonadFail m where
  fail = throwIO . Failed
  {-# INLINE fail #-}

#ifdef VERSION_exceptions
instance {-# OVERLAPPABLE #-} MonadThrow m => MonadFail m where
  fail = throwM . Failed
  {-# INLINE fail #-}
#endif

instance {-# OVERLAPPING #-} MonadFail STM where
  fail _ = retry
  {-# INLINE fail #-}

instance {-# OVERLAPPING #-} MonadFail (Either String) where
  fail = Left
  {-# INLINE fail #-}

instance {-# OVERLAPPING #-} MonadFail Option where
  fail _ = Option Nothing
  {-# INLINE fail #-}

#ifdef VERSION_data_default
instance {-# OVERLAPPABLE #-} (Semigroup m, Default m) => Monoid m where
  mempty = def
  mappend = (<>)
  {-# INLINE mempty #-}

instance {-# OVERLAPPABLE #-} (Monoid m) => Default m where
  def = mempty
  {-# INLINE def #-}
#endif

#ifdef VERSION_unliftio_core
#ifdef VERSION_mtl
-- HOLY BALLS this was a pain in the ass to get to compile...
instance {-# OVERLAPPABLE #-} (Exception e, MonadUnliftIO m) => MonadUnliftIO (ExceptT e m) where
  askUnliftIO = f <$> (lift $ askUnliftIO)
    where
      f innerUIO = UnliftIO $ \(ExceptT m) -> ((unliftIO innerUIO) m >>= \case
              Left e  -> throwIO e
              Right a -> return a
            )
#endif
#endif
