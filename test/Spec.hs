import DOMSpec ( domSpec )
import PWXSpec ( pwxSpec )
import           Test.Hspec

main :: IO ()
main = hspec $ do
  domSpec
  pwxSpec