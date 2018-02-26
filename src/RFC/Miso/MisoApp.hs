{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RFC.Miso.MisoApp
  ( MisoApp(..)
  , run
  , runIso
  , initAndRun
  , initAndRunIso
  ) where

import qualified Miso               as Miso (miso, startApp)
import qualified Miso.Event.Types   as Miso
import qualified Miso.Types         as Miso
import           RFC.Miso.Component
import           RFC.Miso.Routing
import           RFC.Miso.String
import           RFC.Prelude

-- | Typeclass wrapper around 'Miso.App' functionality.
class (Component model, ComponentContainer model) => MisoApp model where
  initialAction :: model -> Action model

  subs :: model -> [Sub (Action model) model]
  subs _ = []
  {-# INLINE subs #-}

  events :: model -> Map MisoString Bool
  events _ = Miso.defaultEvents
  {-# INLINE events #-}

  mountPoint :: model -> Maybe MisoString
  mountPoint _ = Nothing
  {-# INLINE mountPoint #-}


createApp :: model -> Miso.App model action
createApp model = Miso.App
  { Miso.model = model
  , Miso.update = update
  , Miso.view = view
  , Miso.subs = subs model
  , Miso.events = events model
  , Miso.initialAction = initialAction model
  , Miso.mountPoint = mountPoint model
  }
{-# INLINE createApp #-}

-- | Executes the application. If the mountpoint is 'Nothing', then it we will clear the body of the document first.
run :: model -> IO ()
run = doRun Miso.startApp
{-# INLINE run #-}

-- | Executes the application isomorphically. This assumes we already have the pre-rendered DOM in place.
runIso :: (HasURI model) => model -> IO ()
runIso = doRun Miso.miso
{-# INLINE runIso #-}

doRun :: (App model (Action model) -> IO ()) -> model -> IO ()
doRun f = f . createApp
{-# INLINE doRun #-}

initAndRun :: forall app route. (MisoApp app, RouteEmbed app route) => InitArgs app -> [Proxy route] -> IO ()
initAndRun = doInitAndRun Miso.startApp
{-# INLINE initAndRun #-}

initAndRunIso :: forall app route. (MisoApp app, RouteEmbed app route) => InitArgs app -> [Proxy route] -> IO ()
initAndRunIso = doInitAndRun Miso.miso
{-# INLINE initAndRunIso #-}

doInitAndRun ::
  forall app route. (MisoApp app, RouteEmbed app route) =>
  (App model (Action model) -> IO ()) -> InitArgs app -> [Proxy route] -> IO ()
doInitAndRun f initArgs routeProxies = do
    compUpdates <- parallel compModelIOs
    initialModel <- initModel initArgs
    let app = createApp $ foldr ($) initialModel compUpdateIOs
    f app
  where
    routingTable = foldr addRoute newRoutingTable routeProxies
    compUpdateIOs = map (\pxy -> wrapModel <$> toModelIO pxy) components
    toModelIO (ComponentProxy pxy) = initModel $ unwrapInitArgs pxy initArgs
    comps = components appProxy
    appProxy = Proxy :: Proxy app
{-# INLINABLE doInitAndRun #-}
