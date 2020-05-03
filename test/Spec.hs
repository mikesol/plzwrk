import           Control.Monad
import           Control.Monad.Reader
import           Data.IORef
import           Prelude                 hiding ( div )
import           Test.Hspec
import           Web.Framework.Plzwrk
import           Web.Framework.Plzwrk.MockJSVal
import           Data.HashMap.Strict
import           Web.Framework.Plzwrk.Tag       ( p_
                                                , txt
                                                , button
                                                , div'_
                                                )

data MyState = MyState
  { _name :: String
  , _ctr  :: Int
  }

main :: IO ()
main = hspec $ do
  describe "Element with basic state" $ do
    let
      domF =
        (\x y -> div'_
            [ p_ (take y $ repeat (txt (concat [x, show y])))
            , button
              (wId "incr" <.> wOnClick (\_ s -> pure $ s { _ctr = y + 1 }))
              [txt "Increase counter"]
            , button
              (wId "decr" <.> wOnClick (\_ s -> pure $ s { _ctr = y - 1 }))
              [txt "Decrease counter"]
            ]
          )
          <$> _name
          <*> _ctr
    let state = MyState "Mike" 0
    it "Creates the correct DOM from the state" $ do
      rf   <- defaultInternalBrowser
      mock <- makeMockBrowserWithContext rf
      plzwrk' domF state mock
      parentNode     <- documentBody mock
      childrenLevel0 <- (nodeChildNodes mock) parentNode
      length childrenLevel0 `shouldBe` 1
      divtag <- (elementTagName mock) (head childrenLevel0)
      divtag `shouldBe` "div"
      childrenLevel1 <- (nodeChildNodes mock) (head childrenLevel0)
      length childrenLevel1 `shouldBe` 3
      ptag <- (elementTagName mock) (head childrenLevel1)
      ptag `shouldBe` "p"
      childrenLevel2 <- (nodeChildNodes mock) (head childrenLevel1)
      length childrenLevel2 `shouldBe` 0

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
      childrenLevel0' <- (nodeChildNodes mock) parentNode'
      length childrenLevel0' `shouldBe` 1
      divtag' <- (elementTagName mock) (head childrenLevel0')
      divtag' `shouldBe` "div"
      childrenLevel1' <- (nodeChildNodes mock) (head childrenLevel0')
      length childrenLevel1' `shouldBe` 3
      childrenLevel2' <- (nodeChildNodes mock) (head childrenLevel1')
      length childrenLevel2' `shouldBe` 4
      content' <- mapM (nodeTextContent mock) childrenLevel2'
      content' `shouldBe` (take 4 $ repeat "Mike4")
      (documentGetElementById mock) "decr"
        >>= maybe (error "Incr node does not exist") (htmlElemenetClick mock)
      (documentGetElementById mock) "decr"
        >>= maybe (error "Incr node does not exist") (htmlElemenetClick mock)
      parentNode''     <- documentBody mock
      childrenLevel0'' <- (nodeChildNodes mock) parentNode''
      length childrenLevel0'' `shouldBe` 1
      divtag'' <- (elementTagName mock) (head childrenLevel0'')
      divtag'' `shouldBe` "div"
      childrenLevel1'' <- (nodeChildNodes mock) (head childrenLevel0'')
      length childrenLevel1'' `shouldBe` 3
      childrenLevel2'' <- (nodeChildNodes mock) (head childrenLevel1'')
      length childrenLevel2'' `shouldBe` 2
      content'' <- mapM (nodeTextContent mock) childrenLevel2''
      content'' `shouldBe` (take 2 $ repeat "Mike2")
