{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module RFC.Miso.Routing
  ( RoutingURI
  , URI(..)
  , parseURI
  , parseCurrentURI
  , ViewSpec(..)
  , ViewSpecContainer(..)
  , renderViewSpec
  , RouteConfig(..)
  , RouteProxy(..)
  , RoutingTable
  , newRoutingTable
  , addRoute
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
{-# INLINABLE parseURI #-}

parseCurrentURI :: IO RoutingURI
parseCurrentURI = parseURI <$> getCurrentURI
{-# INLINE parseCurrentURI #-}


data ViewSpec parentModel = ViewSpec (RouteProxy parentModel, RoutingURI)

instance Eq (ViewSpec model) where
  (==) (ViewSpec(_,left)) (ViewSpec(_,right)) = left == right

class (Component model) => RouteConfig model where
  runRoute :: model -> RoutingURI -> Maybe (Effect (Action model) model)

class (Component model) => ViewSpecContainer model where
  setViewSpec :: model -> ViewSpec model -> model
  getViewSpec :: model -> ViewSpec model

renderViewSpec :: (ViewSpecContainer parentModel) => parentModel -> View (Action parentModel)
renderViewSpec container =
    case getViewSpec container of
      ViewSpec viewSpec ->
        case viewSpec of
          ( RouteProxy routeModel, _ ) -> viewComponent container routeModel
{-# INLINE renderViewSpec #-}

data RouteProxy parentModel = forall model. (ComponentEmbed parentModel model, RouteConfig model) => RouteProxy (Proxy model)
newtype RoutingTable parentModel = RoutingTable [RouteProxy parentModel]

newRoutingTable :: RoutingTable parentModel
newRoutingTable = RoutingTable []
{-# INLINE newRoutingTable #-}

addRoute :: (ComponentEmbed parentModel model, RouteConfig model) =>
  Proxy model -> RoutingTable parentModel -> RoutingTable parentModel
addRoute pxy (RoutingTable table) =
  RoutingTable $ (RouteProxy pxy):table
{-# INLINE addRoute #-}

runTable ::
  RoutingTable parentModel ->
  (Effect (Action parentModel) parentModel) ->
  parentModel ->
  RoutingURI ->
  Effect (Action parentModel) parentModel
runTable (RoutingTable routes) notFoundEffect parentModel routingURI =
    fromMaybe notFoundEffect $ safeHead routeRunResults
  where
    routeRunResults =
      mapMaybe (\(RouteProxy routePxy) -> do
        model <- unwrapModel routePxy parentModel
        routeEffect <- runRoute model routingURI
        return $ wrapEffect routeEffect parentModel
      ) routes
{-# INLINABLE runTable #-}


