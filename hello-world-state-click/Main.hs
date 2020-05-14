{-# LANGUAGE QuasiQuotes       #-}

import           Web.Framework.Plzwrk
import           Web.Framework.Plzwrk.Asterius
import           Web.Framework.Plzwrk.Tag       ( p__ )

newtype Person = Person { _name :: String }

main :: IO ()
main = do
  browser <- asteriusBrowser
  let elt = (\name -> [hsx'|<div>
    <p>#t{name}#</p>
    <button click=#c{(\_ s -> return $ Person "Bob")}#>Change name</button>
  </div>|])
  plzwrk' (elt <$> _name) (Person "Stacey") browser
