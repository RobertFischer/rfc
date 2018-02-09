{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

-- |Client to access API of Coinhive: https://coinhive.com/documentation/http-api
module RFC.Client.Coinhive
  ( module RFC.Client.Coinhive
  ) where

import           RFC.Prelude

import           Data.Aeson.Types as JSON
import           RFC.API
import           RFC.HTTP.Client  (HasHttpManager (..))
import           RFC.JSON
import           Servant

#ifndef GHCJS_BROWSER
import           Servant.Client
#endif

-- |The secret key given by CoinHive to the client, WHICH SHOULD NEVER BE SHARED.
newtype SecretKey = SecretKey String deriving (FromJSON,ToJSON,ToHttpApiData,FromHttpApiData)

-- |The token id that a user has, which we want to verify.
newtype TokenId = TokenId String deriving (FromJSON,ToJSON,ToHttpApiData,FromHttpApiData)

-- |Response from a token verification request.
data TokenVerification = TokenVerification
  { tvSuccess :: Bool
  , tvHashes  :: Integer
  , tvCreated :: Integer
  , tvError   :: Maybe String
  }
$(deriveJSON jsonOptions ''TokenVerification)

-- |Arguments required to request verification of a token.
data TokenVerifyRequest = TokenVerifyRequest
  { tvrSecret :: SecretKey
  , tvrToken  :: TokenId
  , tvrHashes :: Integer
  }
$(deriveJSON jsonOptions ''TokenVerifyRequest)

-- |The proxy so that you can refer to the API type.
api :: Proxy API
api = Proxy

-- |The unification of the various endpoints.
type API =
  TokenVerify

-- |Endpoint to request verification of a token
type TokenVerify =
  "token" :> "verify" :> JReqBody TokenVerifyRequest :> JPost TokenVerification

#ifndef GHCJS_BROWSER

-- |Monad defining how to request a token verification. See 'tokenVerify' instead: that's probably what you want.
tokenVerifyM :: TokenVerifyRequest -> ClientM TokenVerification
tokenVerifyM
  = client api

-- |Base URL for CoinHive's API
apiUrlBase :: BaseUrl
apiUrlBase = BaseUrl
  { baseUrlScheme = Https
  , baseUrlHost = "api.coinhive.com"
  , baseUrlPort = 443
  , baseUrlPath = "/"
  }

-- |Perform a verification of a token.
tokenVerify :: (MonadUnliftIO m, HasHttpManager m) => TokenVerifyRequest -> m TokenVerification
tokenVerify req = do
  manager <- getHttpManager
  let env = ClientEnv {..}
  result <- liftIO $ runClientM (tokenVerifyM req) env
  case result of
    Left err       -> throwIO err
    Right response -> return response
  where
    baseUrl = apiUrlBase
    cookieJar = Nothing

#endif
