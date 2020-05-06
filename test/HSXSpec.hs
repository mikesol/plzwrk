{-# LANGUAGE QuasiQuotes #-}

module HSXSpec
  ( hsxSpec
  )
where

import           Control.Monad
import           Data.HashMap.Strict           as HM
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
                <a click=#{(\_ x -> return $ x + 41)}#>Hello</a>
            </h1>
        |]
    _elt_tag (dom 3) `shouldBe` "h1"
    _elt_tag (((_elt_children (dom 5)) !! 0) 3) `shouldBe` "a"
    let cf = HM.lookup
          "click"
          (_handlers ((_elt_attrs (((_elt_children (dom 1)) !! 0) 1)) 0))
    res <- maybe (error "Could not find a click function") (\x -> x () 1) cf
    res `shouldBe` 42
  it "Parses hsx with sub-hsx" $ do
    let mylink = [hsx|<a click=#{(\_ x -> return $ x + 41)}#>Hello</a>|]
    let dom = [hsx|
            <h1 id="foo" style="position:absolute">
                #{mylink}#
            </h1>
        |]
    _elt_tag (dom 3) `shouldBe` "h1"
    _elt_tag (((_elt_children (dom 5)) !! 0) 3) `shouldBe` "a"
    1 `shouldBe` 1
  it "Parses hsx'" $ do
    let mylink = [hsx|<a click=#{(\_ x -> return $ x + 41)}#>Hello</a>|]
    let dom = (\st -> [hsx'|
            <h1 id="foo" style="position:absolute">
                #{mylink}#
            </h1>
        |])
    _elt_tag (dom 3) `shouldBe` "h1"
    _elt_tag (((_elt_children (dom 5)) !! 0) 3) `shouldBe` "a"
    1 `shouldBe` 1