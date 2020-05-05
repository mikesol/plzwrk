import DOMSpec ( domSpec )
import HSXSpec ( hsxSpec )
import           Test.Hspec

main :: IO ()
main = hspec $ do
  domSpec
  hsxSpec