{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module RFC.Miso.Component
  ( Component(..)
  , ComponentEmbed(..)
  , ComponentContainer(..)
  , ComponentProxy(..)
  , EmbeddedComponent(..)
  , embeddedComponentToProxy
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

class (Component model, Component parentModel) => ComponentLens parentModel model where
  --initArgsLens :: Lens' (InitArgs parentModel) (InitArgs model)
  unwrapInitArgs :: Proxy model -> InitArgs parentModel -> InitArgs model
  wrapAction     :: model -> Action model -> Action parentModel
  unwrapAction   :: model -> Action parentModel -> Maybe (Action model)
  wrapModel      :: model -> parentModel -> parentModel
  wrapModelPxy   :: Proxy model -> model -> parentModel -> parentModel  -- ^ For cases when we need to help type inference
  wrapModelPxy _ = wrapModel
  unwrapModel    :: Proxy model -> parentModel -> model

data EmbeddedComponent parentModel where
  EmbeddedComponentProxy :: (ComponentEmbed parentModel model) => Proxy model -> EmbeddedComponent parentModel

embeddedComponentToProxy :: EmbeddedComponent parentModel -> (ComponentEmbed parentModel model) => Proxy model
embeddedComponentToProxy (EmbeddedComponentProxy pxy) = pxy
{-# INLINE embeddedComponentToProxy #-}

embeddedComponentToModel :: (ComponentEmbed parentModel model) => EmbeddedComponent parentModel -> parentModel -> model
embeddedComponentToModel = unwrapModel . embeddedComponentToProxy
{-# INLINE embeddedComponentToModel #-}

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
  let model = unwrapModel pxy parentModel
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

class ComponentContainer parentModel where
  components :: Proxy parentModel -> [EmbeddedComponent parentModel]

viewComponent :: (ComponentEmbed parentModel model) => parentModel -> Proxy model -> View (Action parentModel)
viewComponent = wrappedView
{-# INLINE viewComponent #-}

updateComponents ::
  (ComponentContainer parentModel, Component parentModel) =>
  Action parentModel -> parentModel -> Effect (Action parentModel) parentModel
updateComponents action startingParentModel = foldr foldImpl initialEffect comps
  where
    parentPxy = Proxy :: Proxy parentModel
    comps = components parentPxy
    initialEffect = noEff startingParentModel
    foldImpl component (Effect parentModel ios) =
      Effect newParentModel (ios ++ newIos)
        where
          pxy = embeddedComponentToProxy component
          Effect newParentModel newIos = wrappedUpdate pxy action parentModel

{-# INLINABLE updateComponents #-}
