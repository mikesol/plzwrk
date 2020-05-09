{-# LANGUAGE QuasiQuotes       #-}

import           Web.Framework.Plzwrk
import           Web.Framework.Plzwrk.Asterius
import           Web.Framework.Plzwrk.Tag       ( p__ )


main :: IO ()
main = do
  browser <- asteriusBrowser
  plzwrk'_ [hsx|<p>Hello world!</p>|] browser
