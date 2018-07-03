{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RFC.GHCJS.Cookie
	( storePermCookie
	, readCookie
	, ghcjsCookieScript
	) where

import           Data.JSString
import           RFC.Prelude   hiding ( pack )

ghcjsCookieScript :: String
ghcjsCookieScript = "https://cdn.jsdelivr.net/npm/js-cookie@2/src/js.cookie.min.js"

foreign import javascript safe
	"Cookies.set($1,$2,{expires:500})"
	storePermCookie :: JSString -> JSString -> IO ()

readCookie :: (ToText s1, FromText s2) => s1 -> IO (Maybe s2)
readCookie cookieName = do
	raw <- unpack <$> (rawReadCookie .  pack $ toChars cookieName)
	return $ case raw of
		[] -> Nothing
		_ -> Just $ fromChars raw

foreign import javascript safe
	"Cookies.get($1) || ''"
	rawReadCookie :: JSString -> IO JSString
