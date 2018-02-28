{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module RFC.Miso.Component
  ( Component(..)
  , viewComponent
  , updateComponents
  , wrappedView
  , wrappedUpdate
  , wrapEffect
  ) where

import           Control.Lens    hiding (view)
import qualified Control.Lens    as Lens
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

class (Component model, Component parentModel) => ChildComponent parentModel model where
  childInitArgs  :: Getter (InitArgs parentModel) (InitArgs model)
  childAction    :: Lens' (Action parentModel) (Maybe (Action model))
  parentAction   :: Getter (Action model) (Action parentModel)
  childModel     :: Lens' parentModel model

wrapAction :: (ChildComponent parentModel model) => Action model -> Action parentModel
wrapAction action = action ^. parentAction
{-# INLINE wrapAction #-}

wrapActionF :: (ChildComponent parentModel model, Functor f) => f (Action model) -> f (Action parentModel)
wrapActionF = mapped %~ wrapAction
{-# INLINE wrapActionF #-}
{-# SPECIALISE INLINE wrapActionF :: (ChildComponent parentModel model) => [Action model] -> [Action parentModel] #-}
{-# SPECIALISE INLINE wrapActionF :: (ChildComponent parentModel model) => IO (Action model) -> IO (Action parentModel) #-}

wrapActionF2 :: (ChildComponent parentModel model, Functor f1, Functor f2) => f1 (f2 (Action model)) -> f1 (f2 (Action parentModel))
wrapActionF2 = mapped %~ wrapActionF
{-# INLINE wrapActionF2 #-}
{-# SPECIALISE INLINE wrapActionF2 :: (ChildComponent parentModel model) =>  IO [Action model]  ->  IO [Action parentModel]  #-}
{-# SPECIALISE INLINE wrapActionF2 :: (ChildComponent parentModel model) => [IO (Action model)] -> [IO (Action parentModel)] #-}

wrapEffect :: (ChildComponent parentModel model) =>
  Effect (Action model) model -> parentModel -> Effect (Action parentModel) parentModel
wrapEffect (Effect model childIOs) parentModel =
  Effect
    (parentModel & childModel .~ model )
    (wrapActionF2 childIOs)
{-# INLINE wrapEffect #-}

wrappedUpdate :: (ChildComponent parentModel model) =>
  Proxy model ->
  Action parentModel ->
  parentModel ->
  Effect (Action parentModel) parentModel
wrappedUpdate _ parentAction parentModel = fromMaybe (noEff parentModel) $ do
  action <- parentAction ^. childAction :: Maybe (Action model)
  let model = parentModel ^. childModel :: model
  return $ wrapEffect (update action model) parentModel
{-# INLINABLE wrappedUpdate #-}

wrappedView :: (ChildComponent parentModel model) => parentModel -> Proxy model -> View (Action parentModel)
wrappedView parentModel _ =
    parentModel ^. childModel & view & wrapActionF
  where
    doView :: model -> View (Action model)
    doView = view
{-# INLINABLE wrappedView #-}

viewComponent :: (ChildComponent parentModel model) => parentModel -> Proxy model -> View (Action parentModel)
viewComponent = wrappedView
{-# INLINE viewComponent #-}

