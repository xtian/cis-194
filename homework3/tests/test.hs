import Test.Tasty
import Test.Tasty.HUnit

import Golf

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "skips (n=4)" $
      (skips "ABCD") @?= ["ABCD", "BD", "C", "D"]

  , testCase "skips (n=6)" $
      (skips "hello!") @?= ["hello!", "el!", "l!", "l", "o", "!"]

  , testCase "skips (n=1)" $
      (skips [1 :: Int]) @?= [[1 :: Int]]

  , testCase "skips (n=2)" $
      (skips [True, False]) @?= [[True, False], [False]]

  , testCase "skips (n=0)" $
      null (skips []) @?= True
  ]
