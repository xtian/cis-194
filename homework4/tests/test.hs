import Test.Tasty
import Test.Tasty.HUnit

import Homework4

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "fun1' n=0" $
      (fun1' []) @?= 1

  , testCase "fun1' n=2" $
      (fun1' [4, 6]) @?= 8

  , testCase "fun1' n=4" $
      (fun1' [4..7]) @?= 8

  , testCase "fun2' x=1" $
      (fun2' 1) @?= 0

  , testCase "fun2' x=10" $
      (fun2' 10) @?= 40

  , testCase "fun2' x=11" $
      (fun2' 11) @?= 212
  ]
