 {-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}

module RFC.Miso.MisoApp
  ( MisoApp(..)
  , run
  , runIso
  , initAndRun
  , initAndRunIso
  , runRoutingTable
  ) where

import           Data.Proxy
import qualified Miso                      as Miso (Effect (..), miso, startApp)
import qualified Miso.Event.Types          as Miso
import qualified Miso.Html                 as Miso (Sub)
import           Miso.Router               (HasURI)
import           Miso.Subscription.History (uriSub)
import qualified Miso.Types                as Miso
import           RFC.Concurrent
import           RFC.Miso.Component
import           RFC.Miso.Routing
import           RFC.Miso.String
import           RFC.Prelude
import           UnliftIO.Async
import           UnliftIO.Concurrent

-- | Typeclass wrapper around 'Miso.App' functionality.
class (Component model, ComponentContainer model, ViewSpecContainer model) => MisoApp model where
  initialAction :: model -> Action model
  routingAction :: model -> RoutingURI -> Action model
  multipleActions :: [Action model] -> Action model

  setRoutingTable :: RoutingTable model -> model -> model
  getRoutingTable :: model -> RoutingTable model

  subs :: model -> [Miso.Sub (Action model) model]
  subs app =
    [ uriSub (routingAction app . parseURI)
    ]
  {-# INLINE subs #-}

  events :: model -> Map MisoString Bool
  events _ = Miso.defaultEvents
  {-# INLINE events #-}

  mountPoint :: model -> Maybe MisoString
  mountPoint _ = Nothing
  {-# INLINE mountPoint #-}



runRoutingTable :: (MisoApp app) =>
  Miso.Effect (Action app) app -> RoutingURI -> app -> Miso.Effect (Action app) app
runRoutingTable notFoundEffect routingURI app =
    runTable (getRoutingTable app) notFoundEffect app routingURI
{-# INLINE runRoutingTable #-}


createApp :: (MisoApp model) => model -> Miso.App model (Action model)
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
run :: (MisoApp model) => model -> IO ()
run = doRun Miso.startApp
{-# INLINE run #-}

-- | Executes the application isomorphically. This assumes we already have the pre-rendered DOM in place.
runIso :: (MisoApp model, HasURI model) => model -> IO ()
runIso = doRun Miso.miso
{-# INLINE runIso #-}

doRun :: (MisoApp model) => (Miso.App model (Action model) -> IO ()) -> model -> IO ()
doRun f = f . createApp
{-# INLINE doRun #-}

initAndRun :: (MisoApp app) => InitArgs app -> RoutingTable app -> IO ()
initAndRun = doInitAndRun Miso.startApp
{-# INLINE initAndRun #-}

initAndRunIso :: (MisoApp app, HasURI app) => InitArgs app -> RoutingTable app -> IO ()
initAndRunIso = doInitAndRun Miso.miso
{-# INLINE initAndRunIso #-}

doInitAndRun ::
  (MisoApp app, ComponentContainer app) =>
  (Miso.App app (Action app) -> IO ()) -> InitArgs app -> RoutingTable app -> IO ()
doInitAndRun f initArgs routingTable = do
    compUpdates <- mapConcurrently toModelIO $ components (Proxy :: Proxy app)
    initialModel <- initModel initArgs
    let modelWithComps = foldr ($) initialModel compUpdates
    let finalModel = setRoutingTable routingTable modelWithComps
    routingURI <- parseCurrentURI
    let app = createApp finalModel
    f $ app {
      Miso.initialAction = multipleActions
        [ initialAction finalModel
        , routingAction finalModel routingURI
        ]
    }
  where
    toModelIO (ComponentProxy pxy) = do
      let modelInitArgs = unwrapInitArgs pxy initArgs
      model <- initModel modelInitArgs
      return $ wrapModel model

