{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , ComponentMapping(..)
  , ComponentContainer(..)
  , ComponentEffect
  , ComponentTransition
  , ComponentView
  , Subcomponents
  , addComponent
  , viewComponent
  , updateComponents
  , mapComponent
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
  {-# MINIMAL initModel, view, (update|transition) #-}
  data Action model   :: *
  data InitArgs model :: *

  initModel   :: Getter (InitArgs model) (IO model)
  view        :: Getter (model) (ComponentView model)

  update     :: Action model -> model -> ComponentEffect model
  update action = fromTransition $ action ^. transition
  {-# INLINE update #-} -- Only inlines if instance uses default method

  transition :: Getter (Action model) (ComponentTransition model ())
  transition = to $ \action -> toTransition $ update action
  {-# INLINE transition #-} -- Only inlines if instance uses default method

class (Component parent, Component child) => ComponentMapping parent child where
  childInitArgs :: Getter (InitArgs parent) (InitArgs child)
  childAction   :: Getter (Action parent) (Maybe (Action child))
  parentAction  :: Getter (Action child) (Action parent)

class (Component parent) => ComponentContainer parent where
  subcomponents :: Lens' parent (Subcomponents parent)

newtype Subcomponents parent = Subcomponents [StashedComponent parent] deriving (Eq,Monoid,Semigroup,MonoFoldable,MonoFunctor,MonoPointed)
type instance Element (Subcomponents parent) = StashedComponent parent

addComponent :: (ComponentMapping parent child) => child -> Subcomponents parent -> Subcomponents parent
addComponent child (Subcomponents scs) = Subcomponents $ (StashedComponent child):scs
{-# INLINE addComponent #-}

removeStashedComponent :: StashedComponent parent -> Subcomponents parent -> Subcomponents parent
removeStashedComponent stashed =
  ofoldr
    (\sc@(StashedComponent child) memo ->
      if stashed == sc then
        memo
      else
        addComponent child memo
    ) mempty
{-# INLINABLE removeStashedComponent #-}

replaceSubcomponent ::
  (ComponentContainer parent, ComponentMapping parent child) =>
  StashedComponent parent -> child -> parent -> parent
replaceSubcomponent stashed child parent =
    parent & subcomponents .~ newSubs
  where
    oldSubs = parent^.subcomponents
    newSubs = oldSubs & removeStashedComponent stashed . addComponent child
{-# INLINABLE replaceSubcomponent #-}

data StashedComponent parent = forall child. ComponentMapping parent child => StashedComponent child

instance Eq (StashedComponent parent) where
  (==) (StashedComponent left) (StashedComponent right) = cast right & maybe False ((==) left)
  {-# INLINE (==) #-}

newtype SCWrap parent result = SCWrap (forall child. ComponentMapping parent child => child -> result)

applyComponent :: StashedComponent parent -> SCWrap parent result -> result
applyComponent (StashedComponent child) (SCWrap f) = f child
{-# INLINE applyComponent #-}

wrappedView :: (Component parent) => StashedComponent parent -> ComponentView parent
wrappedView (StashedComponent child) =
  child^.view & fmap (\it -> it^.parentAction)
{-# INLINE wrappedView #-}

wrappedUpdate :: (Component parent, ComponentContainer parent) =>
  Action parent -> parent -> StashedComponent parent -> ComponentEffect parent
wrappedUpdate action parent sc =
    applyComponent sc doUpdate
  where
    doUpdate = SCWrap $ \theChild ->
      case action^.childAction of
        Nothing       -> noEff parent
        Just myAction ->
          let Effect newChild childIOs = update myAction theChild in
          Effect
            (replaceSubcomponent sc newChild parent)
            (map (fmap (\it -> it^.parentAction)) childIOs)
{-# INLINABLE wrappedUpdate #-}

updateComponents :: (Component parent, ComponentContainer parent) =>
  Action parent -> parent -> ComponentEffect parent
updateComponents action initialParent =
    foldr foldImpl (noEff initialParent) subs
  where
    Subcomponents subs = initialParent^.subcomponents
    foldImpl sc (Effect prevParent prevActs) =
      let Effect newParent newActs = wrappedUpdate action prevParent sc in
          Effect newParent (newActs ++ prevActs)
{-# INLINABLE updateComponents #-}

viewComponent :: (Component parent, ComponentContainer parent, ComponentMapping parent child) =>
  parent -> Proxy child -> ([ComponentView parent] -> ComponentView parent) -> ComponentView parent
viewComponent parent pxy concatF =
    concatF $ map wrappedView matchingScs
  where
    childRep = typeRep pxy
    matchingScs = subs & filter (\(StashedComponent child) -> childRep == typeOf child)
    Subcomponents subs = parent^.subcomponents
{-# INLINABLE viewComponent #-}

mapComponent :: (Component parent, ComponentContainer parent, ComponentMapping parent child) =>
  (child -> child) -> parent -> parent
mapComponent f parent =
    parent & subcomponents .~ (omap mapImpl $ parent^.subcomponents)
  where
    mapImpl (StashedComponent maybeChild) =
      case cast maybeChild of
        Nothing        -> StashedComponent maybeChild
        Just castChild -> StashedComponent (f castChild)
{-# INLINABLE mapComponent #-}
