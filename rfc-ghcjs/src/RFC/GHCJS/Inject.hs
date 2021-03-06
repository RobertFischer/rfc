{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}

module RFC.GHCJS.Inject
  ( injectJS
  , injectCSS
  , injectStylesheet
  , injectPreconnectHint
  , injectPreconnectHintURI
  , writeDocumentTitle
  ) where

import           Data.JSString
import           Network.URI
import           RFC.Prelude

-- | Injects a String of CSS declarations as an inline <style type="text/css"> element.
injectStylesheet :: String -> IO ()
injectStylesheet = injectStylesheet' . pack
{-# INLINE injectStylesheet #-}

-- | Injects a reference to a JS file as a <script type="text/javascript"> element.
injectJS :: String -> IO ()
injectJS = injectJS' . pack
{-# INLINE injectJS #-}

-- | Injects a reference to a CSS file as a <link rel="stylesheet"> element.
injectCSS :: String -> IO ()
injectCSS = injectCSS' . pack
{-# INLINE injectCSS #-}

-- | Injects a preconnect hint as a <link rel="preconnect"> element.
injectPreconnectHint :: String -> IO ()
injectPreconnectHint str =
  case parseURI str of
    Nothing  -> injectPreconnectHint' str
    Just uri -> injectPreconnectHintURI uri
{-# INLINABLE injectPreconnectHint #-}

-- | Injects a preconnect hint as a <link rel="preconnect"> element.
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
injectPreconnectHint' str         = injectPreconnectHint'' . pack $ str
{-# INLINE injectPreconnectHint' #-}

foreign import javascript safe
  "if(document.title !== $1) { document.title = $1 };"
  writeDocumentTitle :: JSString -> IO ()

foreign import javascript safe
  "var script=document.createElement('link');script.rel='preconnect';script.crossorigin=true;script.href='https://' + $1;document.getElementsByTagName('head')[0].appendChild(script);"
  injectPreconnectHint'' :: JSString -> IO ()

foreign import javascript safe
  "var script=document.createElement('script');script.async='true';script.defer='true';script.type='text/javascript';script.src=$1;document.getElementsByTagName('head')[0].appendChild(script);"
  injectJS' :: JSString -> IO ()

foreign import javascript safe
  "var ss=document.createElement('link');ss.rel='stylesheet';ss.href=$1;ss.type='text/css';document.getElementsByTagName('head')[0].appendChild(ss);"
  injectCSS' :: JSString -> IO ()

foreign import javascript safe
  "var ss=document.createElement('style');ss.type='text/css';ss.innerHTML=$1;document.getElementsByTagName('head')[0].appendChild(ss);"
  injectStylesheet' :: JSString -> IO ()
