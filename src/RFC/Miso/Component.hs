{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module RFC.Miso.Component
  ( Component(..)
  , ComponentContainer(..)
  , ComponentEffect
  , ComponentTransition
  , ComponentView
  , Subcomponents
  , liftComponent
  , consComponent
  , viewComponent
  , viewComponents
  , updateComponents
  , mapComponents
  , foldrComponents
  ) where

import           Control.Lens    hiding (view)
import           Data.Proxy
import           Data.Typeable
import           Miso.Effect
import           Miso.Html       (View (..))
import           Miso.Types      (Transition, fromTransition, toTransition)
import           RFC.Miso.String ()
import           RFC.Prelude

type ComponentEffect model = Effect (Action model) model
type ComponentTransition model = Transition (Action model) model
type ComponentView model = View (Action model)

class (Typeable model, Eq model) => Component model where
  {-# MINIMAL view, (update|transition) #-}
  data Action model   :: *

  view        :: Getter (model) (ComponentView model)

  update     :: Action model -> model -> ComponentEffect model
  update action = fromTransition $ action ^. transition
  {-# INLINE update #-} -- Only inlines if instance uses default method

  transition :: Getter (Action model) (ComponentTransition model ())
  transition = to $ \action -> toTransition $ update action
  {-# INLINE transition #-} -- Only inlines if instance uses default method

class (Component parent) => ComponentContainer parent where
  subcomponents :: Lens' parent (Subcomponents parent)
  childAction   :: forall child. Component child => Prism' (Action parent) (Action child)

parentAction :: (ComponentContainer parent, Component child) => Getter (Action child) (Action parent)
parentAction = re childAction

data StashedComponent parent = forall model. Component model => StashedComponent model

instance Eq (StashedComponent parent) where
  (==) (StashedComponent left) (StashedComponent right) = cast right & maybe False ((==) left)
  {-# INLINE (==) #-}

newtype Subcomponents parent = Subcomponents [StashedComponent parent] deriving (Eq,Monoid,Semigroup,MonoFoldable,MonoFunctor,MonoPointed)
type instance Element (Subcomponents parent) = StashedComponent parent

liftComponent :: (Component model) => model -> Subcomponents parent
liftComponent = Subcomponents . (:[]) . StashedComponent
{-# INLINE liftComponent #-}

consComponent :: (Component model) => model -> Subcomponents parent -> Subcomponents parent
consComponent child (Subcomponents scs) = Subcomponents $ (StashedComponent child):scs
{-# INLINE consComponent #-}

removeStashedComponent :: StashedComponent parent -> Subcomponents parent -> Subcomponents parent
removeStashedComponent stashed =
  ofoldr
    (\sc@(StashedComponent child) memo ->
      if stashed == sc then
        memo
      else
        consComponent child memo
    ) mempty
{-# INLINABLE removeStashedComponent #-}

replaceSubcomponent ::
  (ComponentContainer parent, Component child) =>
  StashedComponent parent -> child -> parent -> parent
replaceSubcomponent stashed child parent =
    parent & subcomponents .~ newSubs
  where
    newSubs = parent ^. subcomponents & removeStashedComponent stashed . consComponent child
{-# INLINABLE replaceSubcomponent #-}

newtype SCWrap result = SCWrap (forall child. Component child => child -> result)

applyComponent :: StashedComponent parent -> SCWrap result -> result
applyComponent (StashedComponent child) (SCWrap f) = f child
{-# INLINE applyComponent #-}

wrappedView :: (ComponentContainer parent) => StashedComponent parent -> ComponentView parent
wrappedView (StashedComponent child) =
  child^.view & fmap (\it -> it^.parentAction)
{-# INLINE wrappedView #-}

wrappedUpdate :: (ComponentContainer parent) =>
  Action parent -> parent -> StashedComponent parent -> ComponentEffect parent
wrappedUpdate action parent sc =
    applyComponent sc doUpdate
  where
    doUpdate = SCWrap $ \theChild ->
      case action^?childAction of
        Nothing       ->
          noEff parent
        Just myAction ->
          let Effect newChild childIOs = update myAction theChild in
          Effect
            (replaceSubcomponent sc newChild parent)
            (map (fmap $ \it -> it^.parentAction) childIOs)
{-# INLINABLE wrappedUpdate #-}

updateComponents :: (ComponentContainer parent) =>
  Action parent -> parent -> ComponentEffect parent
updateComponents action initialParent =
    foldr foldImpl (noEff initialParent) subs
  where
    Subcomponents subs = initialParent^.subcomponents
    foldImpl sc (Effect prevParent prevActs) =
      let Effect newParent newActs = wrappedUpdate action prevParent sc in
          Effect newParent (newActs ++ prevActs)
{-# INLINABLE updateComponents #-}

viewComponent :: (ComponentContainer parent, Component child) => child -> ComponentView parent
viewComponent = wrappedView . StashedComponent
{-# INLINE viewComponent #-}

viewComponents :: (ComponentContainer parent, Component child) =>
  parent -> Proxy child -> ([ComponentView parent] -> ComponentView parent) -> ComponentView parent
viewComponents parent pxy concatViewsF =
    concatViewsF $ map wrappedView matchingScs
  where
    childRep = typeRep pxy
    matchingScs = subs & filter (\(StashedComponent child) -> childRep == typeOf child)
    Subcomponents subs = parent^.subcomponents
{-# INLINABLE viewComponents #-}

mapComponents :: (ComponentContainer parent) =>
  (forall child. Component child => child -> child) -> parent -> parent
mapComponents f parent =
    parent & subcomponents .~ (omap mapImpl $ parent^.subcomponents)
  where
    mapImpl (StashedComponent child) = StashedComponent $ f child
{-# INLINABLE mapComponents #-}

foldrComponents :: (ComponentContainer parent) =>
  parent -> (forall child. Component child => child -> a -> a) -> a -> a
foldrComponents parent f init =
    foldr foldImpl init subs
  where
    Subcomponents subs = parent^.subcomponents
    foldImpl (StashedComponent child) memo = f child memo
{-# INLINE foldrComponents #-}
