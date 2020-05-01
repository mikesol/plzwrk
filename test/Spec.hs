{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Control.Monad.Reader
import           Data.IORef
import           Data.Text               hiding ( head
                                                , length
                                                , take
                                                , singleton
                                                )
import           Prelude                 hiding ( concat
                                                , div
                                                )
import           Test.Hspec
import           Web.Framework.Plzwrk
import           Web.Framework.Plzwrk.MockJSVal
import           Data.HashMap.Strict
import           Web.Framework.Plzwrk.Tag(p_, txt, button, div'_)

data MyState = MyState
  { _name :: Text
  , _ctr  :: Int
  }

main :: IO ()
main = hspec $ do
  describe "Element with basic state" $ do
    let domF =
          (\x y -> div'_
              [ p_ (take y $ repeat (txt (concat [x, pack $ show y])))
              , button
                (pure dats'
                  { _simple   = singleton "id" "incr"
                  , _handlers = singleton "click"
                                          (\_ s -> pure $ s { _ctr = y + 1 })
                  }
                )
                [txt "Increase counter"]
              , button
                (pure dats'
                  { _simple   = singleton "id" "decr"
                  , _handlers = singleton "click"
                                          (\_ s -> pure $ s { _ctr = y - 1 })
                  }
                )
                [txt "Decrease counter"]
              ]
            )
            <$> _name
            <*> _ctr
    let state = MyState "Mike" 0
    it "Creates the correct DOM from the state" $ do
      rf          <- defaultInternalBrowser
      mock        <- makeMockBrowserWithContext rf
      refToOldDom <- newIORef (OldStuff state Nothing)
      parentNode  <- getBody mock
      newDom      <- runReaderT
        (reconcile refToOldDom
                   domF
                   parentNode
                   Nothing
                   (Just $ hydrate state domF)
        )
        mock
      writeIORef refToOldDom (OldStuff state newDom)
      childrenLevel0 <- (getChildren mock) parentNode
      length childrenLevel0 `shouldBe` 1
      divtag <- (getTag mock) (head childrenLevel0)
      divtag `shouldBe` "div"
      childrenLevel1 <- (getChildren mock) (head childrenLevel0)
      length childrenLevel1 `shouldBe` 3
      ptag <- (getTag mock) (head childrenLevel1)
      ptag `shouldBe` "p"
      childrenLevel2 <- (getChildren mock) (head childrenLevel1)
      length childrenLevel2 `shouldBe` 0
      -- increment 4 times
      (getElementById mock) "incr"
        >>= maybe (error "Incr node does not exist") (click mock)
      (getElementById mock) "incr"
        >>= maybe (error "Incr node does not exist") (click mock)
      (getElementById mock) "incr"
        >>= maybe (error "Incr node does not exist") (click mock)
      (getElementById mock) "incr"
        >>= maybe (error "Incr node does not exist") (click mock)
      parentNode'     <- getBody mock
      childrenLevel0' <- (getChildren mock) parentNode'
      length childrenLevel0' `shouldBe` 1
      divtag' <- (getTag mock) (head childrenLevel0')
      divtag' `shouldBe` "div"
      childrenLevel1' <- (getChildren mock) (head childrenLevel0')
      length childrenLevel1' `shouldBe` 3
      childrenLevel2' <- (getChildren mock) (head childrenLevel1')
      length childrenLevel2' `shouldBe` 4
      content' <- mapM (textContent mock) childrenLevel2'
      content' `shouldBe` (take 4 $ repeat "Mike4")
      (getElementById mock) "decr"
        >>= maybe (error "Incr node does not exist") (click mock)
      (getElementById mock) "decr"
        >>= maybe (error "Incr node does not exist") (click mock)
      parentNode''     <- getBody mock
      childrenLevel0'' <- (getChildren mock) parentNode''
      length childrenLevel0'' `shouldBe` 1
      divtag'' <- (getTag mock) (head childrenLevel0'')
      divtag'' `shouldBe` "div"
      childrenLevel1'' <- (getChildren mock) (head childrenLevel0'')
      length childrenLevel1'' `shouldBe` 3
      childrenLevel2'' <- (getChildren mock) (head childrenLevel1'')
      length childrenLevel2'' `shouldBe` 2
      content'' <- mapM (textContent mock) childrenLevel2''
      content'' `shouldBe` (take 2 $ repeat "Mike2")
