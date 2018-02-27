{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module RFC.Miso.Component
  ( Component(..)
  , ComponentEmbed(..)
  , ComponentContainer(..)
  , ComponentProxy(..)
  , EmbeddedComponent(..)
  , viewComponent
  , updateComponents
  , wrappedView
  , wrappedUpdate
  , wrapEffect
  ) where

import           Data.Proxy
import           Miso.Effect
import           Miso.Html       (View (..))
import qualified Miso.Html       as Html
import           Miso.Types      (Transition, fromTransition, toTransition)
import           RFC.Miso.String ()
import           RFC.Prelude


class (Eq model) => Component model where
  {-# MINIMAL initModel, view, (update|transition) #-}
  data Action model   :: *
  data InitArgs model :: *

  initModel   :: InitArgs model -> IO model
  view        :: model -> View (Action model)

  update     :: Action model -> model -> Effect (Action model) model
  update action = fromTransition $ transition action
  {-# INLINE update #-} -- Only inlines if instance uses default method

  transition :: Action model -> Transition (Action model) model ()
  transition action = toTransition $ update action
  {-# INLINE transition #-} -- Only inlines if instance uses default method

class (Component model, Component parentModel) => ComponentEmbed parentModel model where
  unwrapInitArgs :: Proxy model -> InitArgs parentModel -> InitArgs model
  wrapAction     :: model -> Action model -> Action parentModel
  unwrapAction   :: model -> Action parentModel -> Maybe (Action model)
  wrapModel      :: model -> parentModel -> parentModel
  wrapModelPxy   :: Proxy model -> model -> parentModel -> parentModel  -- ^ For cases when we need to help type inference
  wrapModelPxy _ = wrapModel
  unwrapModel    :: Proxy model -> parentModel -> Maybe model

data EmbeddedComponent parentModel = forall model. (ComponentEmbed parentModel model) => EmbeddedComponent model

wrapIOActions :: (ComponentEmbed parentModel model) => model -> [IO (Action model)] -> [IO (Action parentModel)]
wrapIOActions childModel = map (fmap $ wrapAction childModel)
{-# INLINE wrapIOActions #-}

wrapEffect :: (ComponentEmbed parentModel model) => Effect (Action model) model -> parentModel -> Effect (Action parentModel) parentModel
wrapEffect (Effect childModel childIOs) parentModel =
  Effect
    (wrapModel childModel parentModel)
    (wrapIOActions childModel childIOs)
{-# INLINE wrapEffect #-}

wrappedUpdate :: (ComponentEmbed parentModel model) =>
  Proxy model -> Action parentModel -> parentModel -> Effect (Action parentModel) parentModel
wrappedUpdate pxy parentAction parentModel = fromMaybe (noEff parentModel) $ do
  model  <- unwrapModel pxy parentModel
  action <- unwrapAction model parentAction
  return $ wrapEffect (update action model) parentModel
{-# INLINABLE wrappedUpdate #-}

wrappedView :: (ComponentEmbed parentModel model) => parentModel -> Proxy model -> View (Action parentModel)
wrappedView parentModel pxy =
  maybe
    (Html.text $ cs "")
    (\model -> fmap (wrapAction model) $ view model)
    (unwrapModel pxy parentModel)
{-# INLINABLE wrappedView #-}

-- | Wrapper for a 'Proxy' of an embedded component.
data ComponentProxy parentModel = forall model. (ComponentEmbed parentModel model) => ComponentProxy (Proxy model)

class ComponentContainer parentModel where
  components :: Proxy parentModel -> [ComponentProxy parentModel]

viewComponent :: (ComponentEmbed parentModel model) => parentModel -> Proxy model -> View (Action parentModel)
viewComponent = wrappedView
{-# INLINE viewComponent #-}

updateComponents ::
  forall parentModel model. (ComponentEmbed parentModel model, ComponentContainer parentModel) =>
  Action parentModel -> parentModel -> Effect (Action parentModel) parentModel
updateComponents action startingParentModel = foldr foldImpl initialEffect comps
  where
    parentPxy = Proxy :: Proxy parentModel
    comps = components parentPxy :: [ComponentProxy parentModel]
    initialEffect = noEff startingParentModel
    foldImpl (ComponentProxy pxy) (Effect parentModel ios) =
      let Effect newParentModel newIos = wrappedUpdate pxy action parentModel
      in Effect newParentModel (ios ++ newIos)
{-# INLINABLE updateComponents #-}
