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

injectJS :: String -> IO ()
injectJS = injectJS' . cs
{-# INLINABLE injectJS #-}

injectCSS :: String -> IO ()
injectCSS = injectCSS' . cs
{-# INLINABLE injectCSS #-}

injectPreconnectHint :: String -> IO ()
injectPreconnectHint str =
  case parseURI str of
    Nothing  -> injectPreconnectHint' str
    Just uri -> injectPreconnectHintURI uri
{-# INLINABLE injectPreconnectHint #-}

injectPreconnectHintURI :: URI -> IO ()
injectPreconnectHintURI uri =
  case uriAuthority uri of
    Nothing -> return ()
    Just URIAuth{uriRegName} ->
      injectPreconnectHint uriRegName
{-# INLINABLE injectPreconnectHintURI #-}

injectPreconnectHint' :: String -> IO ()
injectPreconnectHint' ""          = return ()
injectPreconnectHint' "localhost" = return ()
injectPreconnectHint' "127.0.0.1" = return ()
injectPreconnectHint' str         = injectPreconnectHint'' . cs $ str

foreign import javascript safe
  "var script=document.createElement('link');script.rel='preconnect';script.crossorigin=true;script.href='https://' + $1;document.getElementsByTagName('head')[0].appendChild(script);"
  injectPreconnectHint'' :: MisoString -> IO ()

foreign import javascript safe
  "var script=document.createElement('script');script.async='true';script.defer='true';script.type='text/javascript';script.src=$1;document.getElementsByTagName('head')[0].appendChild(script);"
  injectJS' :: MisoString -> IO ()

foreign import javascript safe
  "var ss=document.createElement('link');ss.rel='stylesheet';ss.href=$1;ss.type='text/css';document.getElementsByTagName('head')[0].appendChild(ss);"
  injectCSS' :: MisoString -> IO ()
