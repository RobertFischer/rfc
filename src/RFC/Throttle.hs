module RFC.Throttle
  ( createThrottle
  , withThrottle
  , Throttle
  ) where

import RFC.Prelude
import Data.Pool
import GHC.Conc (numCapabilities)

newtype Throttle = Throttle (Pool ())

createThrottle :: (MonadIO m)  => Int -> m Throttle
createThrottle maxSimultaneous = do
    let stripes = min maxSimultaneous $ log2 numCapabilities
    let perStripe = maxPerStripe stripes
    liftIO $ Throttle <$> createPool
      ioUnit
      (const ioUnit)
      stripes
      0.5
      perStripe
  where
    ioUnit = return () :: IO ()
    maxPerStripe :: Int -> Int
    maxPerStripe stripes = max 1 $ maxSimultaneous `quot` stripes
    log2 :: Int -> Int
    log2 x
      | (toInteger x) <= 2 = 1
      | otherwise = 1 + log2 (x `quot` 2)

withThrottle :: (MonadBaseControl IO m) => Throttle -> m b -> m b
withThrottle (Throttle pool) action = withResource pool (const action)
