import Test.Tasty
import Test.Tasty.HUnit

import Calc

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  []
