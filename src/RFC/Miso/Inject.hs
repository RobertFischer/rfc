{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}

module RFC.Miso.Inject
  ( injectJS
  , injectCSS
  , injectPreconnectHint
  , injectPreconnectHintURI
  ) where

import           Network.URI
import           RFC.Miso.String
import           RFC.Prelude

injectJS :: (ConvertibleString s String) => s -> IO ()
injectJS = injectWithPreconnectHint injectJS'
{-# INLINABLE injectJS #-}
{-# SPECIALIZE injectJS :: String -> IO ()     #-}
{-# SPECIALIZE injectJS :: MisoString -> IO () #-}
{-# SPECIALIZE injectJS :: LazyText -> IO ()   #-}
{-# SPECIALIZE injectJS :: URI -> IO ()        #-}

injectCSS :: (ConvertibleString s String) => s -> IO ()
injectCSS = injectWithPreconnectHint injectCSS'
{-# INLINABLE injectCSS #-}
{-# SPECIALIZE injectCSS :: String -> IO ()     #-}
{-# SPECIALIZE injectCSS :: MisoString -> IO () #-}
{-# SPECIALIZE injectCSS :: LazyText -> IO ()   #-}
{-# SPECIALIZE injectCSS :: URI -> IO ()        #-}

injectWithPreconnectHint :: (ConvertibleString s String) => (MisoString -> IO ()) -> s -> IO ()
injectWithPreconnectHint doInject src = do
  uri <- parseURI' src
  injectPreconnectHintURI uri
  doInject . cs $ uri
{-# INLINABLE injectWithPreconnectHint #-}
{-# SPECIALIZE injectWithPreconnectHint :: (MisoString -> IO ()) -> String -> IO ()     #-}
{-# SPECIALIZE injectWithPreconnectHint :: (MisoString -> IO ()) -> MisoString -> IO () #-}
{-# SPECIALIZE injectWithPreconnectHint :: (MisoString -> IO ()) -> LazyText -> IO ()   #-}
{-# SPECIALIZE injectWithPreconnectHint :: (MisoString -> IO ()) -> URI -> IO ()        #-}

injectPreconnectHint :: (ConvertibleString s String) => s -> IO ()
injectPreconnectHint src =
  case parseURI str of
    Nothing  -> injectPreconnectHint' str
    Just uri -> injectPreconnectHintURI uri
  where
    str :: String
    str = cs src
{-# INLINABLE injectPreconnectHint #-}
{-# SPECIALIZE injectPreconnectHint :: String -> IO ()     #-}
{-# SPECIALIZE injectPreconnectHint :: MisoString -> IO () #-}
{-# SPECIALIZE injectPreconnectHint :: LazyText -> IO ()   #-}
{-# SPECIALIZE injectPreconnectHint :: URI -> IO ()        #-}

injectPreconnectHintURI :: URI -> IO ()
injectPreconnectHintURI uri =
  case uriAuthority uri of
    Nothing -> return ()
    Just URIAuth{uriRegName} ->
      injectPreconnectHint uriRegName
{-# INLINABLE injectPreconnectHintURI #-}

parseURI' :: (ConvertibleString s String) => s -> IO URI
parseURI' src =
  case parseURI str of
    Nothing  -> fail $ "Could not parse URI: " ++ str
    Just uri -> return uri
  where
    str :: String
    str = cs src


injectPreconnectHint' :: String -> IO ()
injectPreconnectHint' ""          = return ()
injectPreconnectHint' "localhost" = return ()
injectPreconnectHint' str         = injectPreconnectHint'' . cs $ str


foreign import javascript safe
  "var script=document.createElement('link');script.rel='preconnect';script.crossorigin=true;script.href='https://' + $1;document.getElementsByTagName('head')[0].appendChild(script);"
  injectPreconnectHint'' :: MisoString -> IO ()

foreign import javascript safe
  "var script=document.createElement('script');script.async=true;script.defer=true;script.type='text/javascript';script.src=$1;document.getElementsByTagName('head')[0].appendChild(script);"
  injectJS' :: MisoString -> IO ()

foreign import javascript safe
  "var ss=document.createElement('link');ss.rel='stylesheet';ss.href=$1;ss.type='text/css';document.getElementsByTagName('head')[0].appendChild(ss);"
  injectCSS' :: MisoString -> IO ()
