{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RFC.GHCJS.Cookie
	( storePermCookie
	, storeTransCookie
	, readCookie
	, ghcjsCookieScript
	) where

import           Data.JSString
import           Network.URI
import           RFC.GHCJS.JSString
import           RFC.Prelude

-- | This provides the URI to js.cookie, which is the cookie library that we use.
ghcjsCookieScript :: URI
ghcjsCookieScript =
	fromMaybe
		(error "Could not parse the JS Cookie URI. This is a major bug.")
		(parseURI "https://cdn.jsdelivr.net/npm/js-cookie@2/src/js.cookie.min.js")
{-# INLINEABLE ghcjsCookieScript #-}

-- | Stores an effectively permanent cookie. Since there is no such thing
--   as an actually permanent cookie in the cookie spec, this stores the
--   cookie with an expiration 500 days in the future.
storePermCookie :: (ToText s1, ToText s2) => s1 -> s2 -> IO ()
storePermCookie key val = rawStorePermCookie (toJSString key) (toJSString val)
{-# INLINEABLE storePermCookie #-}
{-# SPECIALIZE INLINE storePermCookie :: JSString -> JSString -> IO () #-}
{-# SPECIALIZE INLINE storePermCookie :: String -> String -> IO () #-}
{-# SPECIALIZE INLINE storePermCookie :: StrictText -> StrictText -> IO () #-}

foreign import javascript safe
	"Cookies.set($1,$2,{expires:500})"
	rawStorePermCookie :: JSString -> JSString -> IO ()

-- | Stores a transient cookie, which will be deleted when the browser is closed.
storeTransCookie :: (ToText s1, ToText s2) => s1 -> s2 -> IO ()
storeTransCookie key val = rawStoreTransCookie (toJSString key) (toJSString val)
{-# INLINEABLE storeTransCookie #-}
{-# SPECIALIZE INLINE storeTransCookie :: JSString -> JSString -> IO () #-}
{-# SPECIALIZE INLINE storeTransCookie :: String -> String -> IO () #-}
{-# SPECIALIZE INLINE storeTransCookie :: StrictText -> StrictText -> IO () #-}

foreign import javascript safe
	"Cookies.set($1,$2)"
	rawStoreTransCookie :: JSString -> JSString -> IO ()

-- | Reads a cookie, whether transient or permanent.
readCookie :: (ToText s1, FromText s2) => s1 -> IO (Maybe s2)
readCookie cookieName = do
	raw <- unpack <$> rawReadCookie (toJSString cookieName)
	return $ case raw of
		[] -> Nothing
		_ -> Just $ fromChars raw
{-# INLINEABLE readCookie #-}
{-# SPECIALIZE INLINE readCookie :: String -> IO (Maybe String) #-}
{-# SPECIALIZE INLINE readCookie :: (ToText s1) => s1 -> IO (Maybe String) #-}
{-# SPECIALIZE INLINE readCookie :: (FromText s2) => String -> IO (Maybe s2) #-}
{-# SPECIALIZE INLINE readCookie :: JSString -> IO (Maybe String) #-}
{-# SPECIALIZE INLINE readCookie :: (FromText s2) => JSString -> IO (Maybe s2) #-}
{-# SPECIALIZE INLINE readCookie :: StrictText -> IO (Maybe String) #-}
{-# SPECIALIZE INLINE readCookie :: (FromText s2) => StrictText -> IO (Maybe s2) #-}

foreign import javascript safe
	"Cookies.get($1) || ''"
	rawReadCookie :: JSString -> IO JSString
