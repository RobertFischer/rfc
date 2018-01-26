{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module RFC.Servant.ApiDoc
  ( apiToHtml
  , apiToAscii
  , apiToSwagger
  ) where

import           Data.Default    (def)
import           RFC.Prelude
import           RFC.Servant
import           RFC.String
import           Servant.Swagger
import qualified Text.Markdown   as MD

apiToHtml :: (HasDocs a) => Proxy a -> Html
apiToHtml = preEscapedToHtml . (MD.markdown mdSettings) . cs . markdown . docs
  where
    mdSettings = def
      { MD.msLinkNewTab = False
      , MD.msAddHeadingId = True
      }

apiToAscii :: (ConvertibleString String s, HasDocs a) => Proxy a -> s
apiToAscii = cs . markdown . docs

apiToSwagger :: (HasSwagger a) => Proxy a -> Swagger
apiToSwagger = toSwagger
