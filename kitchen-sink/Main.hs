{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
#if defined(PLZWRK_ENABLE_ASTERIUS)
{-# LANGUAGE QuasiQuotes #-}


import           Asterius.Types
import           Control.Monad
import           Data.HashMap.Strict     hiding ( null )
import           Data.IORef
import           NeatInterpolation
import qualified Data.Set                      as S
import qualified Data.Text                     as DT
import           Nouns
import           Prelude                 hiding ( div
                                                , span
                                                )
import           Web.Framework.Plzwrk
import           Web.Framework.Plzwrk.Asterius
import           Web.Framework.Plzwrk.Tag
                                         hiding ( main
                                                , main_
                                                , main'_
                                                )
import qualified Web.Framework.Plzwrk.Tag      as T
                                                ( main
                                                , main_
                                                , main'_
                                                )
data MyState = MyState
  { _name               :: String
  , _abstractToConcrete :: [(String, String)]
  , _myNoun             :: String
  }
  deriving Show

main :: IO ()
main = do
  browser <- asteriusBrowser
  -- add some css!
  _head   <- (getHead browser)
  _style  <- (createElement browser) "style"
  _css    <- (createTextNode browser) (DT.unpack myCss)
  (appendChild browser) _style _css
  (appendChild browser) _head _style
  -- here is our "surprise" aphorism
  let surpriseF =
        (\noun -> if (length noun == 0)
            then div'_ []
            else p'__ $ concat ["Life is like", a_n noun, noun]
          )
          <$> _myNoun
  -- here is our input
  let inputF = input
        (wAttr "type" "text" <.> wStyle "box-sizing" "content-box" <.> wOnInput
          (\e s -> do
            opq <- (getOpaque browser) e "target"
            v <- maybe (pure Nothing) (\y -> (getString browser) y "value") opq
            return $ maybe s (\q -> s { _myNoun = q }) v
          )
        )
        []
  -- and here is our main div
  let
    mainDivF =
      (\abstractToConcrete name -> T.main'_
          [ section
              (wClass "content")
              [ h1__ "Aphorism Machine"
              , ul
                (wClass "res")
                (fmap
                  (\(abs, conc) ->
                    (li__ (concat [abs, " is like", a_n conc, conc]))
                  )
                  abstractToConcrete
                )
              , br
              , surpriseF
              , div
                (wStyles [("width", "100%"), ("display", "inline-block")])
                [ button
                  (wId "incr" <.> wClass "dim" <.> wOnClick
                    (\_ s -> do
                      (consoleLog browser)
                        $  "Here is the current state "
                        <> show s
                      concept    <- randAbstract (random01 browser)
                      comparedTo <- randConcrete (random01 browser)
                      let
                        newS = s
                          { _abstractToConcrete = (concept, comparedTo)
                                                    : abstractToConcrete
                          }
                      (consoleLog browser) $ "Here is the new state " <> show newS
                      return $ newS
                    )
                  )
                  [txt "More aphorisms"]
                , button
                  (wId "decr" <.> wClass "dim" <.> wOnClick
                    (\_ s -> pure $ s
                      { _abstractToConcrete = if (null abstractToConcrete)
                                                then []
                                                else tail abstractToConcrete
                      }
                    )
                  )
                  [txt "Less aphorisms"]
                ]
              , inputF
              , p_ [txt "Logged in as: ", span (wClass "username") [txt name]]
              ]
          ]
        )
        <$> _abstractToConcrete
        <*> _name
  let state = MyState "Bob" [] ""
  plzwrk' mainDivF state browser


randFromList :: [String] -> IO Double -> IO String
randFromList l f = do
  z <- f
  let i = round $ (fromIntegral $ length l) * z
  return $ l !! i


a_n :: String -> String
a_n x =
  let hd = take 1 x
  in  if (hd == "a" || hd == "e" || hd == "i" || hd == "o" || hd == "u")
        then " an "
        else " a "

randAbstract :: IO Double -> IO String
randAbstract = randFromList abstract

randConcrete :: IO Double -> IO String
randConcrete = randFromList concrete

myCss = [text|
body {
	margin: 0;
	font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", "Roboto", "Oxygen", "Ubuntu", "Cantarell", "Fira Sans", "Droid Sans", "Helvetica Neue", sans-serif;
	text-rendering: optimizeLegibility;
	-webkit-font-smoothing: antialiased;
}

html,
body {
	height: 100%;
}

body>div:first-child,
body>div:first-child>div:first-child,
body>div:first-child>div:first-child>div {
	height: inherit;
}

input {
	box-sizing: border-box;
	padding: 9.5px 15px;
	border: 0;
	text-align: center;
	border-bottom: 1px solid #d8d8d8;
	font-size: 14px;
	transition: border-bottom-color 100ms ease-in, color 100ms ease-in;
	max-width: 250px;
	border-radius: 0;
}

input:focus {
	outline: none;
	border-color: #000;
}

.dim {
    opacity: 1;
    transition: opacity .15s ease-in;
    cursor: pointer;
}
.dim:hover,
.dim:focus {
    opacity: .5;
    transition: opacity .15s ease-in;
}
.dim:active {
    opacity: .8;
    transition: opacity .15s ease-out;
}

@media (min-width: 768px) {
	input {
		min-width: 300px;
		max-width: 620px;
	}
}

ul {
    list-style: none;
    padding-left: 0;
}

hr {
    margin-top: 15px;
    margin-bottom: 15px;
    width: 70%;
}

main {
	width: 100%;
	height: 100%;
	display: flex;
	justify-content: center;
	align-items: center;
	padding: 20px;
	box-sizing: border-box;
	flex-direction: column;
}

.content {
	text-align: center;
	max-width: 100%;
	-webkit-animation: fadein 2s;
	-moz-animation: fadein 2s;
	-ms-animation: fadein 2s;
	-o-animation: fadein 2s;
	animation: fadein 2s;
}

h1 {
	font-family: 'Montserrat', sans-serif;
	font-weight: normal;
	font-size: 32px;
	text-align: center;
	margin-bottom: 25px;
}

aside {
	display: flex;
	justify-content: center;
	align-items: center;
	padding: 50px 0 40px 0;
	position: absolute;
	bottom: 0;
	left: 0;
	right: 0;
}

aside nav {
	height: 18px;
	display: flex;
	justify-content: center;
	align-items: center;
}

aside nav a {
	font-size: 13px;
	color: #b2b2b2;
	text-decoration: none;
	transition: color 100ms ease-in;
}

aside nav b {
	display: block;
	background: #b2b2b2;
	width: 1px;
	height: 100%;
	margin: 0 10px;
}

.username {
    font-weight: 500;
}

p {
	font-weight: 400;
	font-size: 14px;
	line-height: 24px;
	max-width: 390px;
	text-align: center;
	margin: 14px auto 30px auto;
}

button {
    background-color: rgba(0, 0, 0, 0.671);
    border: none;
    color: white;
    padding: 10px 12px;
    margin: 10px;
    text-align: center;
    border-radius: 12px;
    text-decoration: none;
    display: inline-block;
    font-size: 14px;
  }

@keyframes fadein {
	from {
		opacity: 0;
	}
	to {
		opacity: 1;
	}
}

@-moz-keyframes fadein {
	from {
		opacity: 0;
	}
	to {
		opacity: 1;
	}
}

@-webkit-keyframes fadein {
	from {
		opacity: 0;
	}
	to {
		opacity: 1;
	}
}

@media (max-height: 400px) {
	aside {
		display: none;
	}
}
  |]

# else
main :: IO ()
main = print "If you're using ahc, please set -DPLZWRK_ENABLE_ASTERIUS as a flag to run this executable."
# endif
