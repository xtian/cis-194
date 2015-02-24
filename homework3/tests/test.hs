import Test.Tasty
import Test.Tasty.HUnit

import Golf

main :: IO ()
main = defaultMain unitTests

histogram1 :: String
histogram1 = unlines
  [ " *        "
  , " *        "
  , " *   *    "
  , "=========="
  , "0123456789"
  ]

histogram2 :: String
histogram2 = unlines
  [ "    *     "
  , "    *     "
  , "    * *   "
  , " ******  *"
  , "=========="
  , "0123456789"
  ]

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

  , testCase "localMaxima (n'=2)" $
      (localMaxima [2, 9, 5, 6, 1]) @?= [9, 6]

  , testCase "localMaxima (n'=1)" $
      (localMaxima [2, 3, 4, 1, 5]) @?= [4]

  , testCase "localMaxima (n'=0)" $
      null (localMaxima [1, 2, 3, 4, 5]) @?= True

  , testCase "histogram (n=4)" $
      (histogram [1, 1, 1, 5]) @?= histogram1

  , testCase "histogram (n=11)" $
      (histogram [1, 4, 5, 4, 6, 6, 3, 4, 2, 4, 9]) @?= histogram2
  ]
