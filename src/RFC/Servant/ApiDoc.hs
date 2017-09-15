module RFC.Servant.ApiDoc
  ( apiToHtml
  , apiToPandoc
  , apiToOdt
  , apiToAscii
  , apiToSwagger
  ) where

import RFC.Prelude
import RFC.String
import RFC.Servant
import Text.Pandoc
import Servant.Docs.Pandoc
import Servant.Swagger
import Data.Default (def)
import qualified Text.Markdown as MD

writerOptions :: WriterOptions
writerOptions = def
  { writerTabStop = 4
  , writerSlideVariant = RevealJsSlides
  , writerIncremental = False
  , writerHTMLMathMethod = GladTeX
  , writerNumberSections = True
  , writerSectionDivs = True
  , writerReferenceLinks = True
  , writerWrapText = WrapNone
  , writerCiteMethod = Biblatex
  , writerHtml5 = True
  , writerHtmlQTags = True
  , writerTopLevelDivision = TopLevelPart
  , writerListings = True
  , writerHighlight = True
  , writerTeXLigatures = True
  , writerVerbose = True
  , writerReferenceLocation = EndOfBlock
  }

apiToPandoc :: HasDocs a => Proxy a -> Pandoc
apiToPandoc = pandoc . docs

apiToHtml :: (HasDocs a) => Proxy a -> Html
apiToHtml = preEscapedToHtml . (MD.markdown mdSettings) . cs . markdown . docs
  where
    mdSettings = def
      { MD.msLinkNewTab = False
      , MD.msAddHeadingId = True
      }

apiToOdt :: (ConvertibleString LazyByteString s, HasDocs a, MonadIO m) => Proxy a -> m s
apiToOdt api = liftIO $ cs <$> writeODT writerOptions pdoc
  where
    pdoc = apiToPandoc api

apiToAscii :: (ConvertibleString String s, HasDocs a) => Proxy a -> s
apiToAscii = cs . markdown . docs

apiToSwagger :: (HasSwagger a) => Proxy a -> Swagger
apiToSwagger = toSwagger
