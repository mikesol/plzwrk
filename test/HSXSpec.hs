{-# LANGUAGE QuasiQuotes #-}

module HSXSpec
  ( hsxSpec
  )
where

import           Control.Monad
import qualified Data.HashMap.Strict           as HM
import           Control.Monad.Reader
import           Data.IORef
import           Test.Hspec
import           Web.Framework.Plzwrk
import           Control.Monad.Logger

genericLogger _ _ _ = print

hsxSpec = describe "HSXParser" $ do
  it "Parses simple hsx" $ do
    let dom = [hsx|<p>Hello world!</p>|]
    -- we use () for an empty state

    _elt_tag (dom ()) `shouldBe` "p"
    _tn_text (head (_elt_children (dom ())) ()) `shouldBe` "Hello world!"
  it "Parses hsx with an event listener" $ do
    let dom = [hsx|
            <h1 id="foo" style="position:absolute">
                <a click=#c{(\_ x -> return $ x + 41)}#>Hello</a>
            </h1>
        |]
    _elt_tag (dom 3) `shouldBe` "h1"
    _elt_tag (((_elt_children (dom 5)) !! 0) 3) `shouldBe` "a"
    let attrs     = (_elt_attrs (((_elt_children (dom 1)) !! 0) 1))
    let clickAttr = (filter (\(x, _) -> x == "click") attrs) !! 0
    let mf (PwFunctionAttribute f) = f
    let cf = mf ((snd clickAttr) 0)
    res <- cf () 1
    res `shouldBe` 42
  it "Parses hsx with sub-hsx" $ do
    let mylink = [hsx|<a click=#c{(\_ x -> return $ x + 41)}#>Hello</a>|]
    let dom = [hsx|
            <h1 id="foo" style="position:absolute">
                #e{mylink}#
                #t{"hello world"}#
            </h1>
        |]
    _elt_tag (dom 3) `shouldBe` "h1"
    _elt_tag (head (_elt_children (dom 5)) 3) `shouldBe` "a"
    _tn_text ((_elt_children (dom 5) !! 1) 3) `shouldBe` "hello world"
    1 `shouldBe` 1
  it "Parses terse hsx with sub-hsx" $ do
    let mylink = [hsx|<a click=#c{(\_ x -> return $ x + 41)}#>Hello</a>|]
    let
      dom
        = [hsx|<h1 id="foo" style="position:absolute">#e{mylink}##t{"hello world"}#</h1>|]
    _elt_tag (dom 3) `shouldBe` "h1"
    _elt_tag (head (_elt_children (dom 5)) 3) `shouldBe` "a"
    _tn_text ((_elt_children (dom 5) !! 1) 3) `shouldBe` "hello world"
    1 `shouldBe` 1
  it "Parses hsx with a list of elements" $ do
    let mylink = [hsx|<a click=#c{(\_ x -> return $ x + 41)}#>Hello</a>|]
    let dom = [hsx|
            <h1 id="foo" style="position:absolute">
                #el{take 10 $ repeat mylink}#
                #t{"hello world"}#
            </h1>
        |]
    _elt_tag (dom 3) `shouldBe` "h1"
    _elt_tag (head (_elt_children (dom 5)) 3) `shouldBe` "a"
    _elt_tag ((_elt_children (dom 5) !! 6) 3) `shouldBe` "a"
    _tn_text ((_elt_children (dom 5) !! 10) 3) `shouldBe` "hello world"
  it "Parses hsx mixing text and not text" $ do
    let mylink = [hsx|<a click=#c{(\_ x -> return $ x + 41)}#>Hello</a>|]
    let dom = [hsx|
            <h1 id="foo" style="position:absolute">
                <div>Hello <span>world</span> </div>
            </h1>
        |]
    _elt_tag (dom 3) `shouldBe` "h1"
    _elt_tag (head (_elt_children (dom 5)) 3) `shouldBe` "div"
    _elt_tag ((_elt_children (head (_elt_children (dom 5)) 3) !! 1) 5)
      `shouldBe` "span"
  it "Parses hsx'" $ do
    let mylink = [hsx|<a click=#c{(\_ x -> return $ x + 41)}#>Hello</a>|]
    let dom =
          (\st -> [hsx'|
            <h1 id="foo" style=#t{"position:absolute"}#>
                #e{mylink}#
            </h1>
        |]
          )
    _elt_tag (dom 3) `shouldBe` "h1"
    _elt_tag (head (_elt_children (dom 5)) 3) `shouldBe` "a"
  it "Handles <br />" $ do
    -- we can check this later, for now we just make sure it parses
    v <- runNoLoggingT (parseHSX_ "<br />")
    _hsxSelfClosingTag_tag v `shouldBe` "br"
    _hsxSelfClosingTag_attributes v `shouldBe` []
  it "Handles nested elements" $ do
    -- we can check this later, for now we just make sure it parses
    v <- runNoLoggingT
      (parseHSX_ "<div>  \n  <span>a</span>   hello <span></span>world </div>")
    _hsxElement_tag v `shouldBe` "div"
  it "Handles terse nested elements" $ do
    -- we can check this later, for now we just make sure it parses
    v <- runNoLoggingT
      (parseHSX_ "<div><span>a</span>hello<span></span>world</div>")
    _hsxElement_tag v `shouldBe` "div"
  it "Handles terse nested elements with dynamic text" $ do
    -- we can check this later, for now we just make sure it parses
    v <- runNoLoggingT
      (parseHSX_
        "<div><span>a</span>hello<span>#t{\"there\"}#</span>#t{\"world\"}#</div>"
      )
    _hsxElement_tag v `shouldBe` "div"
  it "Handles double back-to-back brackets" $ do
    -- we can check this later, for now we just make sure it parses
    v <- runNoLoggingT (parseHSX_ "<div click=#c{\\_ s -> s { _foo=1 } }# />")
    _hsxElement_tag v `shouldBe` "div"
  it "Handles double back-to-back brackets with no whitespace" $ do
  -- we can check this later, for now we just make sure it parses
    v <- runNoLoggingT (parseHSX_ "<div click=#c{\\_ s -> s { _foo=1 }}# />")
    _hsxElement_tag v `shouldBe` "div"
