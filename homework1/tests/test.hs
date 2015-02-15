import Test.Tasty
import Test.Tasty.HUnit

import Homework1

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "toDigits (positive input)" $
      (toDigits 1234) @?= [1, 2, 3, 4]

  , testCase "toDigits (zero)" $
      (toDigits 0) @?= []

  , testCase "toDigits (negative input)" $
      (toDigits (-17)) @?= []

  , testCase "toDigitsRev" $
      (toDigitsRev 1234) @?= [4, 3, 2, 1]

  , testCase "doubleEveryOther (n=4)" $
      (doubleEveryOther [8, 7, 6, 5]) @?= [16, 7, 12, 5]

  , testCase "doubleEveryOther (n=3)" $
      (doubleEveryOther [1, 2, 3]) @?= [1, 4, 3]

  , testCase "sumDigits" $
      (sumDigits [16, 7, 12, 5]) @?= 22
  ]
