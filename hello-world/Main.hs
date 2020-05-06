{-# LANGUAGE CPP               #-}
{-# LANGUAGE QuasiQuotes       #-}

import           Web.Framework.Plzwrk
#if defined(PLZWRK_ENABLE_ASTERIUS)
import           Web.Framework.Plzwrk.Asterius
import           Web.Framework.Plzwrk.Tag (p__)
#else
import           Web.Framework.Plzwrk.MockJSVal
#endif

main :: IO ()
main = do
#if defined(PLZWRK_ENABLE_ASTERIUS)
  browser <- asteriusBrowser
# else
  browser <- makeMockBrowser
# endif
  plzwrk'_ [hsx|<p>Hello world!</p>|] browser
