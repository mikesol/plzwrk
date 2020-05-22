{-# LANGUAGE QuasiQuotes #-}

module PWXSpec
  ( pwxSpec
  )
where

import           Control.Monad
import qualified Data.HashMap.Strict           as HM
import           Control.Monad.Reader
import           Data.IORef
import           Test.Hspec
import           Web.Framework.Plzwrk

pwxSpec = describe "PWXParser" $ do
  it "Parses simple pwx" $ do
    let dom = [pwx|<p>Hello world!</p>|]
    -- we use () for an empty state

    _elt_tag (dom ()) `shouldBe` "p"
    _tn_text (head (_elt_children (dom ())) ()) `shouldBe` "Hello world!"
  it "Parses pwx with an event listener" $ do
    let dom = [pwx|
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
  it "Parses pwx with sub-pwx" $ do
    let mylink = [pwx|<a click=#c{(\_ x -> return $ x + 41)}#>Hello</a>|]
    let dom = [pwx|
            <h1 id="foo" style="position:absolute">
                #e{mylink}#
                #t{"hello world"}#
            </h1>
        |]
    _elt_tag (dom 3) `shouldBe` "h1"
    _elt_tag (head (_elt_children (dom 5)) 3) `shouldBe` "a"
    _tn_text ((_elt_children (dom 5) !! 1) 3) `shouldBe` "hello world"
    1 `shouldBe` 1
  it "Parses terse pwx with sub-pwx" $ do
    let mylink = [pwx|<a click=#c{(\_ x -> return $ x + 41)}#>Hello</a>|]
    let
      dom
        = [pwx|<h1 id="foo" style="position:absolute">#e{mylink}##t{"hello world"}#</h1>|]
    _elt_tag (dom 3) `shouldBe` "h1"
    _elt_tag (head (_elt_children (dom 5)) 3) `shouldBe` "a"
    _tn_text ((_elt_children (dom 5) !! 1) 3) `shouldBe` "hello world"
    1 `shouldBe` 1
  it "Parses pwx with a list of elements" $ do
    let mylink = [pwx|<a click=#c{(\_ x -> return $ x + 41)}#>Hello</a>|]
    let dom = [pwx|
            <h1 id="foo" style="position:absolute">
                #el{take 10 $ repeat mylink}#
                #t{"hello world"}#
            </h1>
        |]
    _elt_tag (dom 3) `shouldBe` "h1"
    _elt_tag (head (_elt_children (dom 5)) 3) `shouldBe` "a"
    _elt_tag ((_elt_children (dom 5) !! 6) 3) `shouldBe` "a"
    _tn_text ((_elt_children (dom 5) !! 10) 3) `shouldBe` "hello world"
  it "Parses pwx mixing text and not text" $ do
    let mylink = [pwx|<a click=#c{(\_ x -> return $ x + 41)}#>Hello</a>|]
    let dom = [pwx|
            <h1 id="foo" style="position:absolute">
                <div>Hello <span>world</span> </div>
            </h1>
        |]
    _elt_tag (dom 3) `shouldBe` "h1"
    _elt_tag (head (_elt_children (dom 5)) 3) `shouldBe` "div"
    _elt_tag ((_elt_children (head (_elt_children (dom 5)) 3) !! 1) 5)
      `shouldBe` "span"
  it "Parses pwx'" $ do
    let mylink = [pwx|<a click=#c{(\_ x -> return $ x + 41)}#>Hello</a>|]
    let dom =
          (\st -> [pwx'|
            <h1 id="foo" style=#t{"position:absolute"}#>
                #e{mylink}#
            </h1>
        |]
          )
    _elt_tag (dom 3) `shouldBe` "h1"
    _elt_tag (head (_elt_children (dom 5)) 3) `shouldBe` "a"
  it "Handles <br />" $ do
    -- we can check this later, for now we just make sure it parses
    v <- parsePWX_ "<br />"
    _pwxSelfClosingTag_tag v `shouldBe` "br"
    _pwxSelfClosingTag_attributes v `shouldBe` []
  it "Handles nested elements" $ do
    -- we can check this later, for now we just make sure it parses
    v <- parsePWX_ "<div>  \n  <span>a</span>   hello <span></span>world </div>"
    _pwxElement_tag v `shouldBe` "div"
  it "Handles terse nested elements" $ do
    -- we can check this later, for now we just make sure it parses
    v <- parsePWX_ "<div><span>a</span>hello<span></span>world</div>"
    _pwxElement_tag v `shouldBe` "div"
  it "Handles terse nested elements with dynamic text" $ do
    -- we can check this later, for now we just make sure it parses
    v <- parsePWX_
      "<div><span>a</span>hello<span>#t{\"there\"}#</span>#t{\"world\"}#</div>"
    _pwxElement_tag v `shouldBe` "div"
  it "Handles double back-to-back brackets" $ do
    -- we can check this later, for now we just make sure it parses
    v <- parsePWX_ "<div click=#c{\\_ s -> s { _foo=1 } }# />"
    _pwxSelfClosingTag_tag v `shouldBe` "div"
  it "Handles double back-to-back brackets with no whitespace" $ do
  -- we can check this later, for now we just make sure it parses
    v <- parsePWX_ "<div click=#c{\\_ s -> s { _foo=1 }}# />"
    _pwxSelfClosingTag_tag v `shouldBe` "div"
