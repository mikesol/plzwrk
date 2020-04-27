{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import DomBase
import DomWAI
import Prelude hiding(concat, div)

data MyState = MyState { _name :: Text, _ctr :: Int }

main :: IO ()
main = do
    let elt = (\x y -> div dats [
                pure $ div dats [
                    pure $ txt (concat [x, pack $ show y]) | _ <- [0..9]
                ]
            ]) <$> _name <*> _ctr
    plzwrk (MyState "Mike" 0) elt "mynode"