{-# LANGUAGE QuasiQuotes       #-}

import           Web.Framework.Plzwrk
import           Web.Framework.Plzwrk.Asterius
import           Web.Framework.Plzwrk.Tag       ( p__ )


main :: IO ()
main = do
  browser <- asteriusBrowser
  let element = [hsx|<p>Hello world!</p>|]
  let bigger = [hsx|<div style="background-color:red">
    #el{replicate 10 element}#
  </div>
  |]
  plzwrk'_ bigger browser

