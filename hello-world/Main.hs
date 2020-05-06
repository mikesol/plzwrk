{-# LANGUAGE CPP               #-}
{-# LANGUAGE QuasiQuotes       #-}

import           Web.Framework.Plzwrk
#if defined(PLZWRK_ENABLE_ASTERIUS)
import           Web.Framework.Plzwrk.Asterius
import           Web.Framework.Plzwrk.Tag (p__)

main :: IO ()
main = do
  browser <- asteriusBrowser
  plzwrk'_ [hsx|<p>Hello world!</p>|] browser


# else
main :: IO ()
main = print "If you're using ahc, please set -DPLZWRK_ENABLE_ASTERIUS as a flag to run this executable."
# endif
