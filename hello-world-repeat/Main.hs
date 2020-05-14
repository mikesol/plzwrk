{-# LANGUAGE QuasiQuotes       #-}

import           Web.Framework.Plzwrk
import           Web.Framework.Plzwrk.Asterius
import           Web.Framework.Plzwrk.Tag       ( p__ )

data Person = Person { _name :: String, _age :: Int }

main :: IO ()
main = do
  browser <- asteriusBrowser
  let mystyle = "background-color:pink"
  let element = (\age -> [hsx'|<p>You just turned <span>#t{show age}#</span>. Congrats!</p>|]) <$> _age
  let bigger = (\name age -> [hsx'|<div style=#t{mystyle}#>
    <h1>#t{name}#</h1>
    #el{replicate age element}#
    #e{p__ ":-)"}#
  </div>
  |]) <$> _name <*> _age
  plzwrk' bigger (Person "Joe" 42) browser
