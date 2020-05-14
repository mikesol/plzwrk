{-# LANGUAGE QuasiQuotes       #-}

import           Web.Framework.Plzwrk
import           Web.Framework.Plzwrk.Asterius
import           Web.Framework.Plzwrk.Tag       ( p__ )

main :: IO ()
main = do
  browser <- asteriusBrowser
  let who = "world"
  let mystyle = "background-color:red"
  let element = [hsx|<p>Hello #t{who}#</p>|]
  let bigger = [hsx|<div click=#c{(\e s -> (consoleLogS browser) "clicked")}# style=#t{mystyle}#>
    #el{replicate 10 element}#
    #e{element}#
  </div>
  |]
  plzwrk'_ bigger browser
