{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module RFC.Miso.Routing
  ( module RFC.Miso.Routing
  , URI(..)
  ) where

import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Proxy
import           Miso.Effect
import           Miso.Html                 (View (..))
import           Miso.Subscription.History (URI (..), getCurrentURI)
import qualified Network.URL               as URL
import           RFC.Prelude

newtype URIHash = URIHash String deriving (Eq)
newtype URIQuery = URIQuery (Map String [String]) deriving (Eq)
type RoutingURI = (URIHash, URIQuery)

parseURI :: URI -> RoutingURI
parseURI URI{uriFragment,uriQuery} =
    (URIHash $ parseHash uriFragment, URIQuery $ parseQuery uriQuery)
  where
    parseHash ('#':rest) = parseHash rest
    parseHash ('!':rest) = parseHash rest
    parseHash ('/':rest) = parseHash rest
    parseHash hash       = hash
    parseQuery :: String -> Map String [String]
    parseQuery ('?':rest) = parseQuery rest
    parseQuery query =
      case URL.importParams query of
        Nothing ->
          Map.empty
        Just pairs ->
          Map.map sort $ Map.fromListWith (++) $ map (second listify) pairs
    listify :: String -> [String]
    listify ""  = []
    listify val = [val]

parseCurrentURI :: IO (URIHash,URIQuery)
parseCurrentURI = parseURI <$> getCurrentURI

data ViewSpec parentModel parentAction = ViewSpec (parentModel -> View parentAction, RoutingURI)

instance Eq (ViewSpec model action) where
  (==) (ViewSpec(_,left)) (ViewSpec(_,right)) = left == right

class (Typeable model) => RouteConfig model action
      | model -> action, action -> model
  where
    routeUpdate   :: model -> action -> Effect action model
    routeView     :: model -> View action
    runRoute      :: RoutingURI -> Maybe (Effect action model)

class (RouteConfig model action) => RouteConvert parentModel parentAction model action
      | parentAction -> parentModel, parentModel -> parentAction
  where
    wrapAction    :: action -> model -> parentAction
    unwrapAction  :: parentAction -> model -> Maybe action
    wrapModel     :: parentModel -> model -> parentModel
    unwrapModel   :: parentModel -> model
    wrapEffect    :: parentModel -> Effect action model -> Effect parentAction parentModel
    wrapEffect parentModel (Effect model childIOs) =
      Effect
          (wrapModel parentModel model)
          (map (fmap (\act -> wrapAction act model)) childIOs)

    wrappedRunRoute :: Proxy model -> WrappedRun parentModel parentAction
    wrappedRunRoute _ uriPair currentParentModel = do
        childEffect <- runRoute uriPair
        return (effectWrapper childEffect, ViewSpec (view, uriPair))
      where
        effectWrapper :: Effect action model -> Effect parentAction parentModel
        effectWrapper = wrapEffect currentParentModel
        view :: parentModel -> View parentAction
        view parentModel = fmap (\action -> wrapAction action childModel) $ routeView childModel
          where
            childModel :: model
            childModel = unwrapModel parentModel

    wrappedRouteUpdate :: Proxy model -> WrappedUpdate parentModel parentAction
    wrappedRouteUpdate _ parentModel parentAction =
      case unwrapAction parentAction model of
        Nothing     -> noEff parentModel
        Just action -> wrapEffect parentModel $ routeUpdate model action
      where
        model :: model
        model = unwrapModel parentModel

type WrappedRun parentModel parentAction =
  RoutingURI -> parentModel -> Maybe (Effect parentAction parentModel, ViewSpec parentModel parentAction)

type WrappedUpdate parentModel parentAction = parentModel -> parentAction -> Effect parentAction parentModel

data RoutingTable parentModel parentAction =
  RoutingTable [(WrappedRun parentModel parentAction, WrappedUpdate parentModel parentAction)]

addRoute :: (RouteConvert parentModel parentAction model action) =>
  RoutingTable parentModel parentAction -> Proxy model -> RoutingTable parentModel parentAction
addRoute (RoutingTable table) pxy =
  RoutingTable $ (wrappedRunRoute pxy, wrappedRouteUpdate pxy):table

runTable ::
  RoutingTable parentModel parentAction ->
  (parentModel -> View parentAction) ->
  RoutingURI ->
  parentModel ->
  (Effect parentAction parentModel, ViewSpec parentModel parentAction)
runTable (RoutingTable routes) notFoundView routingURI parentModel =
    case safeHead $ catMaybes routeRunResults of
      Nothing -> (noEff parentModel, ViewSpec (notFoundView, routingURI))
      Just results -> results
  where
    routeRunResults = map (\run -> run routingURI parentModel) $ map fst routes

updateTable ::
  RoutingTable parentModel parentAction ->
  parentModel ->
  parentAction ->
  Effect parentAction parentModel
updateTable (RoutingTable tbl) initialParentModel parentAction =
    foldr doMerge initialEffect updateCalls
  where
    initialEffect = Effect initialParentModel []
    updateCalls = map snd tbl
    doMerge call (Effect parentModel ios) =
      let (Effect newParentModel moreIOs) = call parentModel parentAction in
      Effect newParentModel (ios++moreIOs)


