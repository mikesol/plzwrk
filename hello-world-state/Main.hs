{-# LANGUAGE QuasiQuotes       #-}

import           Web.Framework.Plzwrk
import           Web.Framework.Plzwrk.Asterius
import           Web.Framework.Plzwrk.Tag       ( p__ )

newtype Person = Person { _name :: String }

main :: IO ()
main = do
  browser <- asteriusBrowser
  let elt = (\name -> [hsx'|<p>#t{name}#</p>|])
  plzwrk' (elt <$> _name) (Person "Stacey") browser
