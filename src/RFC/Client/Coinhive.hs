{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

-- |Client to access API of Coinhive: https://coinhive.com/documentation/http-api
module RFC.Client.Coinhive
  ( module RFC.Client.Coinhive
  ) where

import           RFC.Prelude

import           Data.Aeson.Types as JSON
import           RFC.API
import           RFC.JSON
import           Servant

#ifndef GHCJS_BROWSER
import           Servant.Client
#endif

newtype SecretKey = SecretKey String deriving (FromJSON,ToJSON)
newtype TokenId = TokenId String deriving (FromJSON,ToJSON)

data TokenVerification = TokenVerification
  { tvSuccess :: Bool
  , tvHashes  :: Integer
  , tvCreated :: Integer
  , tvError   :: Maybe String
  }
$(deriveJSON jsonOptions ''TokenVerification)

data TokenVerifyRequest = TokenVerifyRequest
  { tvrSecret :: SecretKey
  , tvrToken  :: TokenId
  , tvrHashes :: Integer
  }
$(deriveJSON jsonOptions ''TokenVerifyRequest)

data UserCurrentBalance = UserCurrentBalance
  { ucbSuccess   :: Bool
  , ucbName      :: String
  , ucbTotal     :: Integer
  , ucbWithdrawn :: Integer
  , ucbBalance   :: Integer
  , ucbError     :: Maybe String
  }
$(deriveJSON jsonOptions ''UserCurrentBalance)

data UserWithdrawRequest = UserWithdrawRequest
  { uwrSecret :: SecretKey
  , uwrName   :: String
  , uwrAmount :: Integer
  }
$(deriveJSON jsonOptions ''UserWithdrawRequest)

data UserWithdrawl = UserWithdrawl
  { uwSuccess :: Bool
  , uwName    :: String
  , uwAmount  :: Integer
  , uwError   :: Maybe String
  }
$(deriveJSON jsonOptions ''UserWithdrawl)

data UserOrdering =
  TotalUserOrdering
  | BalanceUserOrdering
  | WithdrawnUserOrdering

instance FromJSON UserOrdering where
  parseJSON = withText "UserOrdering" $ \v ->
    case cs $ toLower v of
      "total"     -> return TotalUserOrdering
      "balance"   -> return BalanceUserOrdering
      "withdrawn" -> return WithdrawnUserOrdering
      _           -> typeMismatch "UserOrdering" (JSON.String v)

instance ToJSON UserOrdering where
  toJSON TotalUserOrdering     = toJSON "total"
  toJSON BalanceUserOrdering   = toJSON "balance"
  toJSON WithdrawnUserOrdering = toJSON "withdrawn"

-- |Represents a single user in a 'UserTopReport' or 'UserListReport'
data ReportUser = ReportUser
  { ruName      :: String
  , ruTotal     :: Integer
  , ruWithdrawn :: Integer
  , ruBalance   :: Integer
  }
$(deriveJSON jsonOptions ''ReportUser)

-- |Report of top users by 'UserOrdering'.
data UserTopReport = UserTopReport
  { utrSuccess :: Bool
  , utrUsers   :: [ReportUser]
  , utrError   :: Maybe String
  }
$(deriveJSON jsonOptions ''UserTopReport)

data UserListReport = UserListReport
  { ulrSuccess  :: Bool
  , ulrUsers    :: [ReportUser]
  , ulrNextPage :: Maybe String
  , ulrError    :: Maybe String
  }
$(deriveJSON jsonOptions ''UserListReport)

data UserResetRequest = UserResetRequest
  { urreqSecret :: SecretKey
  , urreqName   :: String
  }
$(deriveJSON jsonOptions ''UserResetRequest)

data UserResetResult = UserResetResult
  { urrSuccess :: Bool
  , urrError   :: Maybe String
  }
$(deriveJSON jsonOptions ''UserResetResult)


api :: Proxy API
api = Proxy

-- |The unification of the various endpoints.
type API =
  TokenVerify
  :<|> UserBalance
  :<|> UserWithdraw
  :<|> UserTop
  :<|> UserList
  :<|> UserReset

type TokenVerify =
  "token" :> "verify" :> JReqBody TokenVerifyRequest :> JPost TokenVerification

type UserBalance =
  "user" :> "balance" :> QueryParam "secret" SecretKey :> QueryParam "name" String :> JGet UserCurrentBalance

type UserWithdraw =
  "user" :> "withdraw" :> JReqBody UserWithdrawRequest :> JPost UserWithdrawl

type UserTop =
  "user" :> "top" :> QueryParam "secret" SecretKey :> QueryParam "count" Integer :> QueryParam "order" UserOrdering :> JGet UserTopReport

type UserList =
  "user" :> "list" :> QueryParam "secert" SecretKey :> QueryParam "count" Integer :> QueryParam "page" String :> JGet UserListReport

type UserReset =
  "user" :> "reset" :> JReqBody UserResetRequest :> JPost UserResetResult

#ifndef GHCJS_BROWSER

-- |The URL prefix used for Coinhive's API
baseUrl :: BaseUrl
baseUrl = BaseUrl
  { baseUrlScheme = Https
  , baseUrlHost = "api.coinhive.com"
  , baseUrlPort = 443
  , baseUrlPath = "/"
  }

#endif
