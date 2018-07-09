{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module RFC.GHCJS.Cookie
	( storePermCookie
	, storeTransCookie
	, readCookie
	, deleteCookie
	, ghcjsCookieScript
	, newSession
	, updateSession
	, readSession
	, deleteSession
	) where

import           Data.Aeson         as JSON
import           Data.JSString
import           GHCJS.Marshal
import           GHCJS.Nullable
import           GHCJS.Types
import           Network.URI
import           RFC.Data.IdAnd
import           RFC.Data.UUID      as UUID
import           RFC.GHCJS.JSString
import           RFC.Prelude
import           System.Random      ( randomIO )

-- | The key that we use for the cookie that holds the current session id.
sessionIdKey :: JSString
sessionIdKey = toJSString "session_id"
{-# INLINE sessionIdKey #-}

-- | This provides the URI to js.cookie, which is the cookie library that we use.
ghcjsCookieScript :: URI
ghcjsCookieScript =
	fromMaybe
		(error "Could not parse the JS Cookie URI. This is a major bug.")
		(parseURI "https://cdn.jsdelivr.net/npm/js-cookie@2/src/js.cookie.min.js")
{-# INLINEABLE ghcjsCookieScript #-}

-- | Deletes a cookie. Passes silently if the cookie does not exist.
deleteCookie :: (ToText key) => key -> IO ()
deleteCookie = rawDeleteCookie . toJSString
{-# INLINE deleteCookie #-}

foreign import javascript safe
	"Cookies.remove($1)"
	rawDeleteCookie :: JSString -> IO ()

-- | Stores an effectively permanent cookie. Since there is no such thing
--   as an actually permanent cookie in the cookie spec, this stores the
--   cookie with an expiration 500 days in the future.
storePermCookie :: (ToText s1, ToJSON s2) => s1 -> s2 -> IO ()
storePermCookie key val = do
	jsVal <- toJSVal_aeson val
	rawStorePermCookie (toJSString key) jsVal
{-# INLINEABLE storePermCookie #-}

foreign import javascript safe
	"Cookies.set($1,$2,{expires:500})"
	rawStorePermCookie :: JSString -> JSVal -> IO ()

-- | Stores a transient cookie, which will be deleted when the browser is closed.
storeTransCookie :: (ToText s1, ToJSON s2) => s1 -> s2 -> IO ()
storeTransCookie key val = do
	jsVal <- toJSVal_aeson val
	rawStoreTransCookie (toJSString key) jsVal
{-# INLINEABLE storeTransCookie #-}

foreign import javascript safe
	"Cookies.set($1,$2)"
	rawStoreTransCookie :: JSString -> JSVal -> IO ()

-- | Reads a cookie, whether transient or permanent.
readCookie :: (ToText s1, FromJSON s2) => s1 -> IO (Maybe s2)
readCookie cookieName = do
	raw <- rawReadCookie (toJSString cookieName)
	return $ do
		jsonStr <- nullableToMaybe raw
		decode $ toUTF8 jsonStr
{-# INLINEABLE readCookie #-}

foreign import javascript safe
	"Cookies.get($1)"
	rawReadCookie :: JSString -> IO (Nullable JSString)

-- | Generates a random session UUID, stores it into a transient cookie under the name given by 'sessionIdKey'
--   conforming to most of the OWASP suggestions
--   ( https://www.owasp.org/index.php/Session_Management_Cheat_Sheet#Cookies ). Returns the UUID as
--   an 'IdAnd'. Note that it does not set the @HttpOnly@ cookie flag, since that makes it impossible
--   for 'readSession' to work.
newSession :: (ToText cookieDomain, ToJSON sessionData) => cookieDomain -> sessionData -> IO (IdAnd sessionData)
newSession domain sessionData = do
		uuid <- randomIO
		updateSession domain (valuesToIdAnd (Id uuid) sessionData)
{-# INLINEABLE newSession #-}

-- | Updates an existing session for the given UUID, storing it into a transient cookie under the
--   name given by 'sessionIdKey' and conforming to most of the OWASP suggestions
--   ( https://www.owasp.org/index.php/Session_Management_Cheat_Sheet#Cookies ). Returns the UUID as
--   an 'IdAnd'. Note that it does not set the @HttpOnly@ cookie flag, since that makes it impossible
--   for 'readSession' to work.
updateSession :: (ToText cookieDomain, ToJSON sessionData) => cookieDomain -> IdAnd sessionData -> IO (IdAnd sessionData)
updateSession cookieDomain entity@(IdAnd(Id uuid, sessionData)) = do
		let cookieDomJSStr = toJSString cookieDomain
		encodedSessionData <- toJSVal_aeson sessionData
		encodedUuid <- toJSVal_aeson uuid
		rawStoreSession cookieDomJSStr (toJSString uuid) encodedSessionData -- Do this first to ensure consistent state
		rawStoreSession cookieDomJSStr (toJSString sessionIdKey) encodedUuid
		return entity
{-# INLINEABLE updateSession #-}

-- | Checks the cookies for a session, returning the id for the session if it exists.
readSession :: (FromJSON sessionData) => IO (Maybe (IdAnd sessionData))
readSession = do
	maybeId <- readCookie sessionIdKey
	case maybeId >>= UUID.fromString of -- Trip to/from String ensures that session_id is right format
		Nothing -> return Nothing
		Just uuid -> do
			maybeSessionData <- readCookie (UUID.toString uuid)
			return $ case maybeSessionData of
				Nothing -> Nothing
				Just sessionData -> do
					decodedData <- decode $ toUTF8 (sessionData::StrictText)
					return $ IdAnd (Id uuid, decodedData)
{-# INLINEABLE readSession #-}

deleteSession :: IO ()
deleteSession = do
	maybeSession <- readCookie sessionIdKey
	case maybeSession of
		Nothing -> return ()
		Just (sessionIdVal::JSON.Value) ->
			let sessionIdResult = fromJSON sessionIdVal in
			case sessionIdResult of
				Error msg -> fail msg
				Success (sessionId::UUID) -> deleteCookie (toJSString sessionId)
	deleteCookie sessionIdKey
{-# INLINEABLE deleteSession #-}

foreign import javascript safe
	"Cookies.set($2, $3, {secure:window.location.protocol === 'https:',domain:$1,SameSite:true})"
	rawStoreSession :: JSString -> JSString -> JSVal -> IO ()

