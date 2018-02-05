module RFC.Miso.Inject
  ( injectJS
  , injectCSS
  ) where

import           Miso.String
import           RFC.Prelude

foreign import javascript safe
	"var script=document.createElement('script');script.async=true;script.defer=true;script.type='text/javascript';script.src=$1;document.getElementsByTagName('head')[0].appendChild(script);"
	injectJS :: MisoString -> IO ()

foreign import javascript safe
	"var ss=document.createElement('link');ss.rel='stylesheet';ss.href=$1;ss.type='text/css';document.getElementsByTagName('head')[0].appendChild(ss);"
	injectCSS :: MisoString -> IO ()
