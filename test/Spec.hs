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
  _v <- (nodeChildNodes b v)
  maybe (error "Could not find child nodes") pure _v

nodeTextContentOrThrow :: Browserful jsval -> jsval -> IO String
nodeTextContentOrThrow b v = do
  _v <- (nodeTextContent b v)
  maybe (error "Could not find text content") pure _v

elementTagNameOrThrow :: Browserful jsval -> jsval -> IO String
elementTagNameOrThrow b v = do
  _v <- (elementTagName b v)
  maybe (error "Could not find tag name") pure _v

data MyState = MyState
  { _name :: String
  , _ctr  :: Int
  }

main :: IO ()
main = hspec $ do
  describe "Element with basic state" $ do
    let domF =
          (\x y -> div'_
              [ p (wStyle "position" "absolute")
                  (take y $ repeat (txt (concat [x, show y])))
              , button
                (wId "incr" <.> wClasses ["a b ccc"] <.> wOnClick
                  (\_ s -> pure $ s { _ctr = y + 1 })
                )
                [txt "Increase counter"]
              , br
              ,button
                (   wId "decr"
                <.> wStyles [("position", "absolute"), ("margin", "10px")]
                <.> wOnClick (\_ s -> pure $ s { _ctr = y - 1 })
                )
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
      childrenLevel0 <- (nodeChildNodesOrThrow mock) parentNode
      length childrenLevel0 `shouldBe` 1
      divtag <- (elementTagNameOrThrow mock) (head childrenLevel0)
      divtag `shouldBe` "div"
      childrenLevel1 <- (nodeChildNodesOrThrow mock) (head childrenLevel0)
      length childrenLevel1 `shouldBe` 4
      ptag <- (elementTagNameOrThrow mock) (head childrenLevel1)
      ptag `shouldBe` "p"
      childrenLevel2 <- (nodeChildNodesOrThrow mock) (head childrenLevel1)
      length childrenLevel2 `shouldBe` 1

      -- increment 4 times

      (documentGetElementById mock) "incr"
        >>= maybe (error "Incr node does not exist") (htmlElemenetClick mock)
      (documentGetElementById mock) "incr"
        >>= maybe (error "Incr node does not exist") (htmlElemenetClick mock)
      (documentGetElementById mock) "incr"
        >>= maybe (error "Incr node does not exist") (htmlElemenetClick mock)
      (documentGetElementById mock) "incr"
        >>= maybe (error "Incr node does not exist") (htmlElemenetClick mock)
      parentNode'     <- documentBody mock
      childrenLevel0' <- (nodeChildNodesOrThrow mock) parentNode'
      length childrenLevel0' `shouldBe` 1
      divtag' <- (elementTagNameOrThrow mock) (head childrenLevel0')
      divtag' `shouldBe` "div"
      childrenLevel1' <- (nodeChildNodesOrThrow mock) (head childrenLevel0')
      length childrenLevel1' `shouldBe` 4
      childrenLevel2' <- (nodeChildNodesOrThrow mock) (head childrenLevel1')
      length childrenLevel2' `shouldBe` 5
      content' <- mapM (nodeTextContentOrThrow mock) childrenLevel2'
      content' `shouldBe` (take 5 $ repeat "Mike5")
      (documentGetElementById mock) "decr"
        >>= maybe (error "Incr node does not exist") (htmlElemenetClick mock)
      (documentGetElementById mock) "decr"
        >>= maybe (error "Incr node does not exist") (htmlElemenetClick mock)
      parentNode''     <- documentBody mock
      childrenLevel0'' <- (nodeChildNodesOrThrow mock) parentNode''
      length childrenLevel0'' `shouldBe` 1
      divtag'' <- (elementTagNameOrThrow mock) (head childrenLevel0'')
      divtag'' `shouldBe` "div"
      childrenLevel1'' <- (nodeChildNodesOrThrow mock) (head childrenLevel0'')
      length childrenLevel1'' `shouldBe` 4
      childrenLevel2'' <- (nodeChildNodesOrThrow mock) (head childrenLevel1'')
      length childrenLevel2'' `shouldBe` 3
      content'' <- mapM (nodeTextContentOrThrow mock) childrenLevel2''
      content'' `shouldBe` (take 3 $ repeat "Mike3")
      toHTML domF state `shouldBe` "<div><p style=\"position:absolute\">Mike1</p><button class=\"a b ccc\" id=\"incr\">Increase counter</button><br/><button style=\"margin:10px;position:absolute\" id=\"decr\">Decrease counter</button></div>"
