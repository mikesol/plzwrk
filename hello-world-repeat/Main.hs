{-# LANGUAGE QuasiQuotes       #-}

import           Web.Framework.Plzwrk
import           Web.Framework.Plzwrk.Asterius
import           Web.Framework.Plzwrk.Tag       ( p__ )

data Person = Person { _name :: String, _age :: Int }

main :: IO ()
main = do
  browser <- asteriusBrowser
  let mystyle = "background-color:blue"
  let element = (\age -> [hsx'|<p>You are #t{show age} years old</p>|]) <$> _age
  let bigger = (\name age -> [hsx'|<div style=#t{mystyle}#>
    <p>#t{name}#</p>
    #el{replicate age element}#
    #e{element}#
  </div>
  |]) <$> _name <*> _age
  plzwrk' bigger (Person "Joe" 42) browser
