{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module RFC.Miso.Component
  ( Component(..)
  , WrappedComponent
  , ComponentWrapping(..)
  , wrapComponent
  ) where

import           Control.Lens    hiding (view)
import           Miso.Effect
import           Miso.Html       (View (..))
import           Miso.Types      (Transition, fromTransition, toTransition)
import           RFC.Miso.String ()
import           RFC.Prelude

class (Eq model) => Component model where
  {-# MINIMAL initModel, view, (update|transition) #-}
  data Action model   :: *
  data InitArgs model :: *

  initModel   :: Getter (InitArgs model) (IO model)
  view        :: Getter (model) (View (Action model))

  update     :: Action model -> model -> Effect (Action model) model
  update action = fromTransition $ action ^. transition
  {-# INLINE update #-} -- Only inlines if instance uses default method

  transition :: Getter (Action model) (Transition (Action model) model ())
  transition = to $ \action -> toTransition $ update action
  {-# INLINE transition #-} -- Only inlines if instance uses default method

data WrappedComponent parentModel = WrappedComponent
  { wrappedView   :: parentModel -> View (Action parentModel)
  , wrappedUpdate :: Action parentModel -> parentModel -> Effect (Action parentModel) parentModel
  }

data ComponentWrapping parentModel model = ComponentWrapping
  { childModel   :: Lens' parentModel model
  , parentAction :: Action model -> Action parentModel
  , childAction  :: Action parentModel -> Maybe (Action model)
  }

wrapComponent :: (Component parentModel, Component model) =>
  ComponentWrapping parentModel model -> WrappedComponent parentModel
wrapComponent ComponentWrapping{childModel,parentAction,childAction} =
    WrappedComponent { wrappedView, wrappedUpdate }
  where
    wrappedView parentModel = parentModel^.childModel.view & fmap parentAction
    wrappedUpdate action parentModel =
      case childAction action of
        Nothing -> noEff parentModel
        Just myAction ->
          let Effect newChild childIOs = parentModel^.childModel & update myAction in
          Effect
            (parentModel & childModel.~newChild)
            (fmap (fmap parentAction) childIOs)
{-# INLINABLE wrapComponent #-}
