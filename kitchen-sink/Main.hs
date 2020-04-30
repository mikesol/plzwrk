{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
#if defined(PLZWRK_ENABLE_ASTERIUS)

import           Asterius.Types
import           Control.Monad
import           Data.HashMap.Strict
import           Data.IORef
import           Data.Text               hiding ( singleton
                                                , take
                                                )
import           Prelude                 hiding ( concat
                                                , div
                                                )
import           Web.Framework.Plzwrk
import           Web.Framework.Plzwrk.Asterius

data MyState = MyState
  { _name :: Text
  , _ctr  :: Int
  , _ipt  :: Text
  }
  deriving Show

main :: IO ()
main = do
  browser <- asteriusBrowser
  let inputF =
        (\x -> div'_
            [ input
              (pure dats'
                { _simple   = singleton "type" "text"
                , _handlers = singleton
                                "input"
                                (\e s -> do
                                  opq <- (getOpaque browser) e "target"
                                  v   <- (getString browser) e "value"
                                  return $ maybe s (\q -> s { _ipt = q }) v
                                )
                }
              )
              []
            , p_ [txt x]
            ]
          )
          <$> _ipt
  let
    domF =
      (\x y -> div'_
          [ div_ (take y $ repeat (p_ [(txt (concat ["Hello ", x, "!"]))]))
          , button
            (pure dats'
              { _simple   = singleton "id" "incr"
              , _handlers = singleton
                "click"
                (\_ s -> do
                  (consoleLog browser) $ "Here is the current state " <> pack
                    (show s)
                  return $ s { _ctr = (y + 1) }
                )
              }
            )
            [txt "Increase counter"]
          , button
            (pure dats'
              { _simple   = singleton "id" "decr"
              , _handlers = singleton "click"
                                      (\_ s -> pure $ s { _ctr = (y - 1) })
              }
            )
            [txt "Decrease counter"]
          , inputF
          ]
        )
        <$> _name
        <*> _ctr
  let state = MyState "Meeshkan" 0 ""
  plzwrk' domF state browser
# else
main :: IO ()
main = print "If you're using ahc, please set -DPLZWRK_ENABLE_ASTERIUS as a flag to run this executable."
# endif
