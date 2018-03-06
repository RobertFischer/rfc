{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module RFC.Miso.Routing
  ( URI(..)
  , RoutePath
  , RouteQuery
  , RouteSegment
  , routeToURI
  , parseURI
  , parseCurrentURI
  , ViewSpec
  , Routes
  , singletonRoutes
  , addRoute
  , RouteEngine(..)
  , Route(..)
  , applyRoutes
  , applyRoutesToURI
  , applyRoutesIO
  , viewRoutes
  , updateRoutes
  ) where

import           Control.Lens
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Typeable             (cast)
import           Miso.Effect
import           Miso.Html                 (View (..))
import           Miso.Subscription.History (URI (..), getCurrentURI)
import qualified Network.URL               as URL
import           RFC.Miso.Component
import           RFC.Prelude

type RouteSegment = StrictText
type RouteQuery = Map StrictText [StrictText]
type RoutePath = ([RouteSegment], RouteQuery, URI)

routeToURI :: RoutePath -> URI
routeToURI (segs, qry, uri) =
  uri
    { uriQuery = '?':query
    , uriFragment = '#':frag
    }
  where
    encodeString :: ConvertibleStrings a String => Bool -> a -> String -- Second argument is: "Is this a query string?"
    encodeString spaceToPlus = URL.encString spaceToPlus URL.ok_param . cs
    query :: String
    query = intercalate "&" $ pairsToTerms $ Map.toList qry
    pairsToTerms [] = []
    pairsToTerms ((key,[]):rest)       = (encodeString True key) : pairsToTerms rest
    pairsToTerms ((key,(val:[])):rest) = (encodeString True key ++ "=" ++ (encodeString True val)) : pairsToTerms rest
    pairsToTerms ((key,vals):rest)     = (encodeString True key ++ "=" ++ intercalate "," (encodeString True <$> vals)) : pairsToTerms rest
    frag :: String
    frag = intercalate "/" (segToPath <$> segs)
    segToPath :: RouteSegment -> String
    segToPath seg = encodeString False seg
{-# INLINABLE routeToURI #-}

parseURI :: URI -> RoutePath
parseURI uri@URI{uriFragment,uriQuery} =
      ( pathToSegments . parseHash $ uriFragment
      , parseQuery uriQuery
      , uri
      )
  where
    decodeString :: String -> StrictText
    decodeString str = cs . fromMaybe str . URL.decString True $ str
    pathToSegments :: String -> [RouteSegment]
    pathToSegments "" = []
    pathToSegments ('/':rest) = pathToSegments rest
    pathToSegments path = (decodeString start) : pathToSegments rest
      where
        (start,rest) = splitOnSlash path
        splitOnSlash "" = ("","")
        splitOnSlash ('/':content) = splitOnSlash content
        splitOnSlash content =
          case span ((/=) '/') content of
            ([],[])               -> ("","")
            ([], _:theRest)       -> splitOnSlash theRest
            (theStart, [])        -> (theStart, "")
            (theStart, _:theRest) -> (theStart, theRest)
    parseHash ('#':rest) = parseHash rest
    parseHash ('!':rest) = parseHash rest
    parseHash ('/':rest) = parseHash rest
    parseHash hash       = hash
    parseQuery ('?':rest) = parseQuery rest
    parseQuery query =
      case URL.importParams query of
        Nothing ->
          Map.empty
        Just pairs ->
          map (second listify) pairs &
          Map.fromListWith (++) &
          Map.map sort &
          Map.map (fmap cs) &
          Map.mapKeys cs
    listify :: String -> [String]
    listify ""        = []
    listify (',':val) = listify val
    listify val       = start : listify rest
      where
        (start,rest) = splitOnComma val
        splitOnComma ""            = ("","")
        splitOnComma (',':content) = splitOnComma content
        splitOnComma (' ':content) = splitOnComma content
        splitOnComma content =
          case span ((/=) ',') content of
            ([], [])              -> ("", "")
            ([], _:theRest)       -> splitOnComma theRest
            (theStart, [])        -> (theStart, "")
            (theStart, _:theRest) -> (theStart, theRest)
{-# INLINABLE parseURI #-}

parseCurrentURI :: IO RoutePath
parseCurrentURI = parseURI <$> getCurrentURI
{-# INLINE parseCurrentURI #-}

data WrappedRoute = forall route. Route route => WrappedRoute route

instance Eq WrappedRoute where
  (==) (WrappedRoute left) (WrappedRoute right) = cast left & maybe False ((==) right)
  {-# INLINE (==) #-}

newtype Routes = Routes [WrappedRoute] deriving (Eq,Monoid,Semigroup,MonoFoldable,MonoFunctor,MonoPointed)
type instance Element Routes = WrappedRoute

singletonRoutes :: Route model => model -> Routes
singletonRoutes model = Routes [WrappedRoute model]
{-# INLINE singletonRoutes #-}

addRoute :: Route model => model -> Routes -> Routes
addRoute model (Routes routes) = Routes ((WrappedRoute model):routes)
{-# INLINE addRoute #-}

newtype ViewSpec = ViewSpec (WrappedRoute, RoutePath) deriving (Eq)

class (Component model) => RouteEngine model where
  viewSpecAction :: model -> ViewSpec -> Action model
  viewSpec       :: model -> Maybe ViewSpec
  routes         :: Lens' model Routes
  routeAction    :: forall route. Route route => model -> Prism' (Action model) (RouteAction route)

class (Eq model, Typeable model) => Route model where
  data RouteAction model :: *
  data RouteKey    model :: *
  pathKey     :: model -> RoutePath -> Maybe (RouteKey model)
  routeKey    :: model -> RouteKey model
  fetchRoute  :: RouteKey model -> IO model
  viewRoute   :: model -> View (RouteAction model)
  updateRoute :: RouteAction model -> model -> Effect (RouteAction model) model
  keyToPath   :: RouteKey model -> RoutePath

updateRoutes :: (RouteEngine parent) => Action parent -> parent ->  Effect (Action parent) parent
updateRoutes action parent =
    Effect newParent parentIOs
  where
    newParent = parent & routes.~(Routes newRoutes)
    Effect newRoutes parentIOs = foldr doFoldRoutes (noEff []) (parent^.routes)
    doFoldRoutes wr@(WrappedRoute (route::route)) (Effect routes ios) =
      fromMaybe (Effect (wr:routes) ios) $ do
        theAction <- action^?(routeAction parent) :: Maybe (RouteAction route)
        let Effect newRoute newRouteIOs = updateRoute theAction route
        case null newRouteIOs && newRoute == route of
          True -> Nothing
          False -> do
            let newIOs = map (fmap (\act -> act^.(re $ routeAction parent))) newRouteIOs
            return $ Effect ((WrappedRoute newRoute):routes) (newIOs ++ ios)
{-# INLINE updateRoutes #-}

applyRoutes :: (RouteEngine parent, Route notFound) =>
  RouteKey notFound -> RoutePath -> parent -> IO (Action parent)
applyRoutes notFoundKey path parent =
    fromMaybe fetchNotFound runRoutes
  where
    fetchNotFound =
      (viewSpecAction parent)
      <$> (\route -> ViewSpec (WrappedRoute route, path))
      <$> fetchRoute notFoundKey
    Routes myRoutes = parent^.routes
    runRoutes = foldr doRunRoutes Nothing myRoutes
    doRunRoutes (WrappedRoute route) memo = runRoute <|> memo
      where
        runRoute = do
          key <- pathKey route path
          return
            $ viewSpecAction parent
            <$> (\route -> ViewSpec (WrappedRoute route, path))
            <$> fetchRoute key
{-# INLINE applyRoutes #-}

viewRoutes :: (RouteEngine parent, Route notFound) => notFound -> parent -> View (Action parent)
viewRoutes notFound parent =
    case viewSpec parent of
      Nothing -> fmap (\it -> it^.re (routeAction parent)) (viewRoute notFound)
      (Just (ViewSpec (WrappedRoute route, _))) -> fmap (\it -> it^.re (routeAction parent)) (viewRoute route)
{-# INLINE viewRoutes #-}

applyRoutesToURI :: (RouteEngine parent, Route notFound) => RouteKey notFound -> URI -> parent -> IO (Action parent)
applyRoutesToURI notFoundKey uri parent =
    let parsed = parseURI uri in
    applyRoutes notFoundKey parsed parent
{-# INLINE applyRoutesToURI #-}

applyRoutesIO :: (RouteEngine parent, Route notFound) => RouteKey notFound -> parent -> IO (Action parent)
applyRoutesIO notFoundKey parent = do
  routePath <- parseCurrentURI
  applyRoutes notFoundKey routePath parent
{-# INLINE applyRoutesIO #-}
