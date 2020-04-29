{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
#if defined(PLZWRK_ENABLE_ASTERIUS)

import           Asterius.Types
import           Control.Monad
import           Data.IORef
import           Data.Text
import           DomBase
import           DomWAI
import           Prelude        hiding (concat, div)
import           Typeclasses
import           WebAPI

data MyState = MyState
    { _name :: Text
    , _ctr  :: Int
    }

main :: IO ()
main = do
    let elt = (\x y -> div dats [
                pure $ div dats [
                    pure $ txt (concat [x, pack $ show y]) | _ <- [0..9]
                ]
            ]) <$> _name <*> _ctr
    plzwrk (MyState "Mike" 0) elt "mynode"
# else
main :: IO ()
main = print "If you're using ahc, please set -DPLZWRK_ENABLE_ASTERIUS as a flag to run this executable."
# endif