{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module RFC.Miso.Component
  ( Component(..)
  , ComponentEmbed(..)
  ) where

import           Data.Proxy
import           Miso.Effect
import           Miso.Html   (View (..))
import           RFC.Prelude


class Component model where
  data Action model :: *
  type InitArgs model :: *
  initModel :: InitArgs model -> Effect (Action model) model
  update    :: model -> Action model -> Effect (Action model) model
  view      :: model -> View (Action model)

class (Component model, Component parentModel) => ComponentEmbed parentModel model where
  wrapAction   :: model -> Action model -> Action parentModel
  unwrapAction :: model -> Action parentModel -> Maybe (Action model)
  wrapModel    :: model -> parentModel -> parentModel
  unwrapModel  :: Proxy model -> parentModel -> model

  wrapIOActions :: model -> [IO (Action model)] -> [IO (Action parentModel)]
  wrapIOActions !childModel = map (fmap $ wrapAction childModel)

  wrapEffect  :: Effect (Action model) model -> parentModel -> Effect (Action parentModel) parentModel
  wrapEffect (Effect !childModel !childIOs) !parentModel =
    Effect
      (wrapModel childModel parentModel)
      (wrapIOActions childModel childIOs)

  wrappedUpdate :: Proxy model -> parentModel -> Action parentModel -> Effect (Action parentModel) parentModel
  wrappedUpdate !pxy !parentModel !parentAction =
    case unwrapAction model parentAction of
      Nothing ->
        noEff parentModel
      Just action ->
        wrapEffect (update model action) parentModel
    where
      !model = unwrapModel pxy parentModel

  wrappedView :: Proxy model -> parentModel -> View (Action parentModel)
  wrappedView !pxy !parentModel = fmap (wrapAction model) $ view model
    where
      !model = unwrapModel pxy parentModel

