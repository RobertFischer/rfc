{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module RFC.Miso.Routing
  ( RoutingURI
  , URI(..)
  , parseURI
  , parseCurrentURI
  , ViewSpec(..)
  , ViewSpecContainer(..)
  , renderViewSpec
  , RouteConfig(..)
  , RouteEmbed(..)
  , RoutingTable
  , newRoutingTable
  , addRoute
  , updateTable
  , runTable
  ) where

import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Proxy
import           Miso.Effect
import           Miso.Html                 (View (..))
import           Miso.Subscription.History (URI (..), getCurrentURI)
import qualified Network.URL               as URL
import           RFC.Miso.Component
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

parseCurrentURI :: IO RoutingURI
parseCurrentURI = parseURI <$> getCurrentURI

data ViewSpec parentModel = ViewSpec (parentModel -> View (Action parentModel), RoutingURI)

instance Eq (ViewSpec model) where
  (==) (ViewSpec(_,!left)) (ViewSpec(_,!right)) = left == right

class (Component model) => RouteConfig model where
  runRoute :: model -> RoutingURI -> Maybe (Effect (Action model) model)

class (ComponentEmbed parentModel model, RouteConfig model, ViewSpecContainer parentModel) => RouteEmbed parentModel model where
  wrappedRunRoute :: Proxy model -> WrappedRun parentModel
  wrappedRunRoute pxy uriPair currentParentModel = do
      childEffect <- runRoute childSeed uriPair
      return $ effectWrapper childEffect (ViewSpec (view, uriPair))
    where
      childSeed :: model
      childSeed = unwrapModel pxy currentParentModel
      view :: parentModel -> View (Action parentModel)
      view = wrappedView pxy
      effectWrapper :: Effect (Action model) model -> ViewSpec parentModel -> Effect (Action parentModel) parentModel
      effectWrapper childEffect viewSpec = wrapEffect childEffect newParentModel
        where
          newParentModel :: parentModel
          newParentModel = setViewSpec currentParentModel viewSpec

class (Component model) => ViewSpecContainer model where
  setViewSpec :: model -> ViewSpec model -> model
  getViewSpec :: model -> ViewSpec model

renderViewSpec :: (ViewSpecContainer parentModel) => parentModel -> View (Action parentModel)
renderViewSpec container =  renderFunc container
  where
    (ViewSpec (renderFunc,_)) = getViewSpec container

type WrappedRun parentModel =
  RoutingURI -> parentModel -> Maybe (Effect (Action parentModel) parentModel)

type WrappedUpdate parentModel =
  parentModel -> Action parentModel -> Effect (Action parentModel) parentModel

data RoutingTable parentModel =
  RoutingTable [(WrappedRun parentModel, WrappedUpdate parentModel)]

newRoutingTable :: RoutingTable parentModel
newRoutingTable = RoutingTable []

addRoute :: (RouteEmbed parentModel model) =>
  Proxy model -> RoutingTable parentModel -> RoutingTable parentModel
addRoute !pxy (RoutingTable !table) =
  RoutingTable $ (wrappedRunRoute pxy, wrappedUpdate pxy):table

runTable :: (ViewSpecContainer parentModel) =>
  RoutingTable parentModel ->
  (parentModel -> View (Action parentModel)) ->
  RoutingURI ->
  parentModel ->
  Effect (Action parentModel) parentModel
runTable (RoutingTable !routes) !notFoundView !routingURI !parentModel =
    case safeHead $ catMaybes routeRunResults of
      Nothing -> (noEff $ setViewSpec parentModel (ViewSpec (notFoundView, routingURI)))
      Just results -> results
  where
    routeRunResults = map (\run -> run routingURI parentModel) $ map fst routes

updateTable ::
  RoutingTable parentModel ->
  parentModel ->
  Action parentModel->
  Effect (Action parentModel) parentModel
updateTable (RoutingTable !tbl) !initialParentModel !parentAction =
    foldr doMerge initialEffect updateCalls
  where
    !initialEffect = Effect initialParentModel []
    !updateCalls = map snd tbl
    doMerge !call (Effect !parentModel !ios) =
      let (Effect !newParentModel !moreIOs) = call parentModel parentAction in
      Effect newParentModel (ios++moreIOs)


