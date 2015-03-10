import Test.Tasty
import Test.Tasty.HUnit

import Calc
import ExprT

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "eval" $
      (eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))) @?= 20
  ]
