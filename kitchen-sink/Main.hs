{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

#if defined(PLZWRK_ENABLE_ASTERIUS)
import           Asterius.Types
import           Web.Framework.Plzwrk.Asterius
#else
import           Web.Framework.Plzwrk.MockJSVal
#endif
import           Control.Monad
import           Data.HashMap.Strict     hiding ( null )
import           Data.IORef
import qualified Data.Set                      as S
import qualified Data.Text                     as DT
import           Nouns
import           Prelude                 hiding ( div
                                                , span
                                                )
import           Web.Framework.Plzwrk
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

-- here is where we'll show our "surprise" aphorism
surprise =
  (\noun -> if (length noun == 0)
      then div'_ []
      else p'__ $ concat ["Life is like", indefiniteArticle noun, noun]
    )
    <$> _myNoun

-- here is where we will input a noun for our "surprise" aphorosim
writeSomethingConcrete browser = input
  [ ("type" , pT "text")
  , ("style", pT "box-sizing:content-box")
  , ( "input"
    , pF
      (\e s -> do
        v <- (eventTargetValue browser) e
        return $ maybe s (\q -> s { _myNoun = q }) v
      )
    )
  ]
  []

aphorismList =
  (\a2c -> ul'
      [("class", pT "res")]
      (fmap (\(a, c) -> (li__ (concat [a, " is like", indefiniteArticle c, c])))
            a2c
      )
    )
    <$> _abstractToConcrete

addAphorismButton browser =
  (\a2c -> button'
      [ ("id"   , pT "incr")
      , ("class", pT "dim")
      , ( "click"
        , pF
          (\e s -> do
            (eventTargetBlur browser) e
            (consoleLogS browser) $ "Here is the current state " <> show s
            concept    <- randAbstract (mathRandom browser)
            comparedTo <- randConcrete (mathRandom browser)
            let newS = s { _abstractToConcrete = (concept, comparedTo) : a2c }
            (consoleLogS browser) $ "Here is the new state " <> show newS
            return $ newS
          )
        )
      ]
      [txt "More aphorisms"]
    )
    <$> _abstractToConcrete

removeAphorismButton browser =
  (\a2c -> button'
      [ ("id"   , pT "decr")
      , ("class", pT "dim")
      , ( "click"
        , pF
          (\e s -> do
            (eventTargetBlur browser) e
            pure $ s { _abstractToConcrete = if null a2c then [] else tail a2c }
          )
        )
      ]
      [txt "Less aphorisms"]
    )
    <$> _abstractToConcrete

loginText =
  (\name ->
      p'_ [txt "Logged in as: ", span [("class", pT "username")] [txt name]]
    )
    <$> _name

main :: IO ()
main = do
#if defined(PLZWRK_ENABLE_ASTERIUS)
  browser <- asteriusBrowser
#else
  browser <- makeMockBrowser
#endif
  -- add some css!
  _head   <- (documentHead browser)
  _style  <- (documentCreateElement browser) "style"
  _css    <- (documentCreateTextNode browser) (unwords myCss)
  (nodeAppendChild browser) _style _css
  (nodeAppendChild browser) _head _style
  -- and here is our main div
  let mainDivF = T.main_
        [ section
            [("class", pT "content")]
            [ h1__ "Aphorism Machine"
            , aphorismList
            , br
            , surprise
            , div [("style", pT "width:100%;display:inline-block")]
                  [addAphorismButton browser, removeAphorismButton browser]
            , writeSomethingConcrete browser
            , loginText
            ]
        ]
  let state = MyState "Bob" [] ""
  plzwrk' mainDivF state browser


randFromList :: [String] -> IO Double -> IO String
randFromList l f = do
  z <- f
  let i = round $ fromIntegral (length l) * z
  return $ l !! i


indefiniteArticle :: String -> String
indefiniteArticle x =
  let hd = take 1 x
  in  if hd == "a" || hd == "e" || hd == "i" || hd == "o" || hd == "u"
        then " an "
        else " a "

randAbstract :: IO Double -> IO String
randAbstract = randFromList abstract

randConcrete :: IO Double -> IO String
randConcrete = randFromList concrete


myCss =
  [ "body {\n"
  , "  margin: 0;\n"
  , "  font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", \"Roboto\", \"Oxygen\", \"Ubuntu\", \"Cantarell\", \"Fira Sans\", \"Droid Sans\", \"Helvetica Neue\", sans-serif;\n"
  , "  text-rendering: optimizeLegibility;\n"
  , "  -webkit-font-smoothing: antialiased;\n"
  , "}\n"
  , "\n"
  , "html,\n"
  , "body {\n"
  , "  height: 100%;\n"
  , "}\n"
  , "\n"
  , "body>div:first-child,\n"
  , "body>div:first-child>div:first-child,\n"
  , "body>div:first-child>div:first-child>div {\n"
  , "  height: inherit;\n"
  , "}\n"
  , "\n"
  , "input {\n"
  , "  box-sizing: border-box;\n"
  , "  padding: 9.5px 15px;\n"
  , "  border: 0;\n"
  , "  text-align: center;\n"
  , "  border-bottom: 1px solid #d8d8d8;\n"
  , "  font-size: 14px;\n"
  , "  transition: border-bottom-color 100ms ease-in, color 100ms ease-in;\n"
  , "  max-width: 250px;\n"
  , "  border-radius: 0;\n"
  , "}\n"
  , "\n"
  , "input:focus {\n"
  , "  outline: none;\n"
  , "  border-color: #000;\n"
  , "}\n"
  , "\n"
  , ".dim {\n"
  , "    opacity: 1;\n"
  , "    transition: opacity .15s ease-in;\n"
  , "    cursor: pointer;\n"
  , "}\n"
  , ".dim:hover,\n"
  , ".dim:focus {\n"
  , "    opacity: .5;\n"
  , "    transition: opacity .15s ease-in;\n"
  , "}\n"
  , ".dim:active {\n"
  , "    opacity: .8;\n"
  , "    transition: opacity .15s ease-out;\n"
  , "}\n"
  , "\n"
  , "@media (min-width: 768px) {\n"
  , "  input {\n"
  , "    min-width: 300px;\n"
  , "    max-width: 620px;\n"
  , "  }\n"
  , "}\n"
  , "\n"
  , "ul {\n"
  , "    list-style: none;\n"
  , "    padding-left: 0;\n"
  , "}\n"
  , "\n"
  , "hr {\n"
  , "    margin-top: 15px;\n"
  , "    margin-bottom: 15px;\n"
  , "    width: 70%;\n"
  , "}\n"
  , "\n"
  , "main {\n"
  , "  width: 100%;\n"
  , "  height: 100%;\n"
  , "  display: flex;\n"
  , "  justify-content: center;\n"
  , "  align-items: center;\n"
  , "  padding: 20px;\n"
  , "  box-sizing: border-box;\n"
  , "  flex-direction: column;\n"
  , "}\n"
  , "\n"
  , ".content {\n"
  , "  text-align: center;\n"
  , "  max-width: 100%;\n"
  , "  -webkit-animation: fadein 2s;\n"
  , "  -moz-animation: fadein 2s;\n"
  , "  -ms-animation: fadein 2s;\n"
  , "  -o-animation: fadein 2s;\n"
  , "  animation: fadein 2s;\n"
  , "}\n"
  , "\n"
  , "h1 {\n"
  , "  font-family: 'Montserrat', sans-serif;\n"
  , "  font-weight: normal;\n"
  , "  font-size: 32px;\n"
  , "  text-align: center;\n"
  , "  margin-bottom: 25px;\n"
  , "}\n"
  , "\n"
  , "aside {\n"
  , "  display: flex;\n"
  , "  justify-content: center;\n"
  , "  align-items: center;\n"
  , "  padding: 50px 0 40px 0;\n"
  , "  position: absolute;\n"
  , "  bottom: 0;\n"
  , "  left: 0;\n"
  , "  right: 0;\n"
  , "}\n"
  , "\n"
  , "aside nav {\n"
  , "  height: 18px;\n"
  , "  display: flex;\n"
  , "  justify-content: center;\n"
  , "  align-items: center;\n"
  , "}\n"
  , "\n"
  , "aside nav a {\n"
  , "  font-size: 13px;\n"
  , "  color: #b2b2b2;\n"
  , "  text-decoration: none;\n"
  , "  transition: color 100ms ease-in;\n"
  , "}\n"
  , "\n"
  , "aside nav b {\n"
  , "  display: block;\n"
  , "  background: #b2b2b2;\n"
  , "  width: 1px;\n"
  , "  height: 100%;\n"
  , "  margin: 0 10px;\n"
  , "}\n"
  , "\n"
  , ".username {\n"
  , "    font-weight: 500;\n"
  , "}\n"
  , "\n"
  , "p {\n"
  , "  font-weight: 400;\n"
  , "  font-size: 14px;\n"
  , "  line-height: 24px;\n"
  , "  max-width: 390px;\n"
  , "  text-align: center;\n"
  , "  margin: 14px auto 30px auto;\n"
  , "}\n"
  , "\n"
  , "button {\n"
  , "    background-color: rgba(0, 0, 0, 0.671);\n"
  , "    border: none;\n"
  , "    color: white;\n"
  , "    padding: 10px 12px;\n"
  , "    margin: 10px;\n"
  , "    text-align: center;\n"
  , "    border-radius: 12px;\n"
  , "    text-decoration: none;\n"
  , "    display: inline-block;\n"
  , "    font-size: 14px;\n"
  , "  }\n"
  , "\n"
  , "@keyframes fadein {\n"
  , "  from {\n"
  , "    opacity: 0;\n"
  , "  }\n"
  , "  to {\n"
  , "    opacity: 1;\n"
  , "  }\n"
  , "}\n"
  , "\n"
  , "@-moz-keyframes fadein {\n"
  , "  from {\n"
  , "    opacity: 0;\n"
  , "  }\n"
  , "  to {\n"
  , "    opacity: 1;\n"
  , "  }\n"
  , "}\n"
  , "\n"
  , "@-webkit-keyframes fadein {\n"
  , "  from {\n"
  , "    opacity: 0;\n"
  , "  }\n"
  , "  to {\n"
  , "    opacity: 1;\n"
  , "  }\n"
  , "}\n"
  , "\n"
  , "@media (max-height: 400px) {\n"
  , "  aside {\n"
  , "    display: none;\n"
  , "  }\n"
  , "}\n"
  ]
