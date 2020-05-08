module DOMSpec ( domSpec ) where

import           Control.Monad
import           Control.Monad.Reader
import           Data.IORef
import           Prelude                 hiding ( div )
import           Test.Hspec
import           Web.Framework.Plzwrk
import           Web.Framework.Plzwrk.MockJSVal
import           Data.HashMap.Strict
import           Web.Framework.Plzwrk.Tag       ( p
                                                , br
                                                , txt
                                                , button
                                                , div'_
                                                )

nodeChildNodesOrThrow :: Browserful jsval -> jsval -> IO [jsval]
nodeChildNodesOrThrow b v = do
  _v <- nodeChildNodes b v
  maybe (error "Could not find child nodes") pure _v

nodeTextContentOrThrow :: Browserful jsval -> jsval -> IO String
nodeTextContentOrThrow b v = do
  _v <- nodeTextContent b v
  maybe (error "Could not find text content") pure _v

elementTagNameOrThrow :: Browserful jsval -> jsval -> IO String
elementTagNameOrThrow b v = do
  _v <- elementTagName b v
  maybe (error "Could not find tag name") pure _v

data MyState = MyState
  { _name :: String
  , _ctr  :: Int
  }

domSpec = describe "Element with basic state" $ do
    let domF =
          (\x y -> div'_
              [ p [("style", pT "position:absolute")]
                  (replicate y (txt (x ++ show y)))
              , button
                [("id", pT "incr"), ("class", pT "a b ccc"), ("click",
                  pF (\_ s -> pure $ s { _ctr = y + 1 })
                )]
                [txt "Increase counter"]
              , br
              ,button
                [("id", pT "decr"), ("style", pT "position:absolute;margin:10px")
                  , ("click", pF (\_ s -> pure $ s { _ctr = y - 1 }))]
                [txt "Decrease counter"]
              ]
            )
            <$> _name
            <*> _ctr
    let state = MyState "Mike" 1
    it "Creates the correct DOM from the state" $ do
      rf   <- defaultInternalBrowser
      mock <- makeMockBrowserWithContext rf
      plzwrk' domF state mock
      parentNode     <- documentBody mock
      childrenLevel0 <- nodeChildNodesOrThrow mock parentNode
      length childrenLevel0 `shouldBe` 1
      divtag <- elementTagNameOrThrow mock (head childrenLevel0)
      divtag `shouldBe` "div"
      childrenLevel1 <- nodeChildNodesOrThrow mock (head childrenLevel0)
      length childrenLevel1 `shouldBe` 4
      ptag <- elementTagNameOrThrow mock (head childrenLevel1)
      ptag `shouldBe` "p"
      childrenLevel2 <- nodeChildNodesOrThrow mock (head childrenLevel1)
      length childrenLevel2 `shouldBe` 1

      -- increment 4 times

      documentGetElementById mock "incr"
        >>= maybe (error "Incr node does not exist") (htmlElemenetClick mock)
      documentGetElementById mock "incr"
        >>= maybe (error "Incr node does not exist") (htmlElemenetClick mock)
      documentGetElementById mock "incr"
        >>= maybe (error "Incr node does not exist") (htmlElemenetClick mock)
      documentGetElementById mock "incr"
        >>= maybe (error "Incr node does not exist") (htmlElemenetClick mock)
      parentNode'     <- documentBody mock
      childrenLevel0' <- nodeChildNodesOrThrow mock parentNode'
      length childrenLevel0' `shouldBe` 1
      divtag' <- elementTagNameOrThrow mock (head childrenLevel0')
      divtag' `shouldBe` "div"
      childrenLevel1' <- nodeChildNodesOrThrow mock (head childrenLevel0')
      length childrenLevel1' `shouldBe` 4
      childrenLevel2' <- nodeChildNodesOrThrow mock (head childrenLevel1')
      length childrenLevel2' `shouldBe` 5
      content' <- mapM (nodeTextContentOrThrow mock) childrenLevel2'
      content' `shouldBe` replicate 5 "Mike5"
      documentGetElementById mock "decr"
        >>= maybe (error "Incr node does not exist") (htmlElemenetClick mock)
      documentGetElementById mock "decr"
        >>= maybe (error "Incr node does not exist") (htmlElemenetClick mock)
      parentNode''     <- documentBody mock
      childrenLevel0'' <- nodeChildNodesOrThrow mock parentNode''
      length childrenLevel0'' `shouldBe` 1
      divtag'' <- elementTagNameOrThrow mock (head childrenLevel0'')
      divtag'' `shouldBe` "div"
      childrenLevel1'' <- nodeChildNodesOrThrow mock (head childrenLevel0'')
      length childrenLevel1'' `shouldBe` 4
      childrenLevel2'' <- nodeChildNodesOrThrow mock (head childrenLevel1'')
      length childrenLevel2'' `shouldBe` 3
      content'' <- mapM (nodeTextContentOrThrow mock) childrenLevel2''
      content'' `shouldBe` replicate 3 "Mike3"
      toHTML domF state `shouldBe` "<div><p style=\"position:absolute\">Mike1</p><button id=\"incr\" class=\"a b ccc\">Increase counter</button><br/><button id=\"decr\" style=\"position:absolute;margin:10px\">Decrease counter</button></div>"