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

  , testCase "evalStr" $
      (evalStr "(2+3)*4") @?= Just 20

  , testCase "evalStr (no parens)" $
      (evalStr "2+3*4") @?= Just 14

  , testCase "evalStr (invalid)" $
      (evalStr "2+3*") @?= Nothing
  ]
