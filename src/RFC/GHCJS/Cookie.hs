{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RFC.GHCJS.Cookie
	( storePermCookie
	, readCookie
	, ghcjsCookieScript
	) where

import           RFC.Miso.String (MisoString)
import           RFC.Prelude

ghcjsCookieScript :: String
ghcjsCookieScript = "https://cdn.jsdelivr.net/npm/js-cookie@2/src/js.cookie.min.js"

foreign import javascript safe
	"Cookies.set($1,$2,{expires:500})"
	storePermCookie :: MisoString -> MisoString -> IO ()

readCookie :: (ConvertibleStrings s1 MisoString, ConvertibleStrings MisoString s2) => s1 -> IO (Maybe s2)
readCookie cookieName = do
	raw <- rawReadCookie $ cs cookieName
	return $ if null raw then Nothing else Just $ cs raw

foreign import javascript safe
	"Cookies.get($1) || ''"
	rawReadCookie :: MisoString -> IO MisoString
