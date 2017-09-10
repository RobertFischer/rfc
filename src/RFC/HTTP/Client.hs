module RFC.HTTP.Client
  ( withAPISession
  , module Network.Wreq.Session
  ) where

import RFC.Prelude
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wreq.Session (Session,withSessionControl)

withAPISession :: (Session -> IO a) -> IO a
withAPISession = withSessionControl Nothing tlsManagerSettings
