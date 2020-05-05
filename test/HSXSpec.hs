module HSXSpec ( hsxSpec ) where

import           Control.Monad
import           Control.Monad.Reader
import           Data.IORef
import           Test.Hspec
import           Web.Framework.Plzwrk.TH.HSX
import           Data.HashMap.Strict

hsxSpec = describe "XMLParser" $ do
    it "Creates the correct DOM from the state" $ do
        let parsed = parseXML "<h1>Hello</h1>"
        parsed `shouldBe` (Right $ Element "h1" [] [Body "Hello"])