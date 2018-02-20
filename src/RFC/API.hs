{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module RFC.API
  ( JDelete
  , JGet
  , JPatch
  , JPost
  , JPut
  , JReqBody
  ) where

import           RFC.Prelude ()

import           Servant.API

-- |JSON DELETE
type JDelete a = Delete '[JSON] a

-- |JSON GET
type JGet a = Get '[JSON] a

-- |JSON PATCH
type JPatch a = Get '[JSON] a

-- |JSON POST
type JPost a = Post '[JSON] a

-- |JSON PUT
type JPut a = Post '[JSON] a

-- |JSON Request Body ('ReqBody')
type JReqBody a = ReqBody '[JSON] a
