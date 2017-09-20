module RFC.HTTP.Client
  ( withAPISession
  , module Network.Wreq.Session
  , module Network.Wreq.Lens
  , module Network.URL
  , module Control.Lens
  , module Network.HTTP.Types.Status
  ) where

import RFC.Prelude
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wreq.Session hiding (withAPISession)
import Network.URL
import Network.Wreq.Lens
import Control.Lens
import Network.HTTP.Types.Status hiding (statusMessage, statusCode)

withAPISession :: (Session -> IO a) -> IO a
withAPISession = withSessionControl Nothing tlsManagerSettings
