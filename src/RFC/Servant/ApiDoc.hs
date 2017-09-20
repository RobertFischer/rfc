module RFC.Servant.ApiDoc
  ( apiToHtml
  , apiToAscii
  , apiToSwagger
  ) where

import RFC.Prelude
import RFC.String
import RFC.Servant
import Servant.Swagger
import Data.Default (def)
import qualified Text.Markdown as MD

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
