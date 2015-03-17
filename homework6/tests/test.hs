import Test.Tasty
import Test.Tasty.HUnit

import Fibonacci

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "fib x=0" $
      (fib 0) @?= 0

  , testCase "fib x=1" $
      (fib 1) @?= 1

  , testCase "fib x=15" $
      (fib 14) @?= 377

  , testCase "fibs1 n=4" $
      (take 4 fibs1) @?= [0, 1, 1, 2]

  , testCase "fibs2 n=8" $
      (take 8 fibs2) @?= [0, 1, 1, 2, 3, 5, 8, 13]
  ]
