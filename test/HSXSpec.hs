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

hsxSpec = describe "HSXParser" $ do
  it "Parses simple hsx" $ do
    let dom = [hsx|<h1>Hello</h1>|]
    -- we use () for an empty state

    _elt_tag (dom ()) `shouldBe` "h1"
    _tn_text (((_elt_children (dom ())) !! 0) ()) `shouldBe` "Hello"
  it "Parses hsx with an event listener" $ do
    let dom = [hsx|
            <h1 id="foo" style="position:absolute">
                <a click=#c{(\_ x -> return $ x + 41)}#>Hello</a>
            </h1>
        |]
    _elt_tag (dom 3) `shouldBe` "h1"
    _elt_tag (((_elt_children (dom 5)) !! 0) 3) `shouldBe` "a"
    let attrs = (_elt_attrs (((_elt_children (dom 1)) !! 0) 1))
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
    _elt_tag (((_elt_children (dom 5)) !! 0) 3) `shouldBe` "a"
    _tn_text (((_elt_children (dom 5)) !! 1) 3) `shouldBe` "hello world"
    1 `shouldBe` 1
  it "Parses hsx'" $ do
    let mylink = [hsx|<a click=#c{(\_ x -> return $ x + 41)}#>Hello</a>|]
    let dom = (\st -> [hsx'|
            <h1 id="foo" style=#t{"position:absolute"}#>
                #e{mylink}#
            </h1>
        |])
    _elt_tag (dom 3) `shouldBe` "h1"
    _elt_tag (((_elt_children (dom 5)) !! 0) 3) `shouldBe` "a"
