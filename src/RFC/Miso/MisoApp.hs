module RFC.Miso.MisoApp
  ( MisoApp(..)
  , run
  , runIso
  ) where

import qualified Miso                      as Miso ( miso, startApp )
import qualified Miso.Event.Types          as Miso
import qualified Miso.Html                 as Miso ( Sub )
import           Miso.Router               ( HasURI )
import           Miso.Subscription.History ( uriSub )
import qualified Miso.Types                as Miso
import           RFC.Miso.Component
import           RFC.Miso.Routing
import           RFC.Miso.String
import           RFC.Prelude

-- | Typeclass wrapper around 'Miso.App' functionality.
class (ComponentContainer model, RouteEngine model) => MisoApp model where
  initialAction :: model -> Action model
  multipleActions :: [Action model] -> Action model
  notFoundRoute :: model -> RouteKey notFound
  uriChangeAction :: model -> Action model

  subs :: model -> [Miso.Sub (Action model) model]
  subs app =
      [ uriSub (const $ uriChangeAction app)
      ]
  {-# INLINE subs #-}

  events :: model -> Map MisoString Bool
  events _ = Miso.defaultEvents
  {-# INLINE events #-}

  mountPoint :: model -> Maybe MisoString
  mountPoint _ = Nothing
  {-# INLINE mountPoint #-}



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

