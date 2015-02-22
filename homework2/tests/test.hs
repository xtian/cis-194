import Test.Tasty
import Test.Tasty.HUnit

import Log
import LogAnalysis

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

sampleLog :: String
sampleLog = unlines
  [ "I 6 Completed armadillo processing"
  , "E 70 3 Way too many pickles"
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "parseMessage (error)" $
      (parseMessage "E 20 562 hlp hlp") @?= LogMessage (Error 20) 562 "hlp hlp"

  , testCase "parseMessage (warning)" $
      (parseMessage "W 5 Check flange") @?= LogMessage Warning 5 "Check flange"

  , testCase "parseMessage (info)" $
      (parseMessage "I 29 la la la") @?= LogMessage Info 29 "la la la"

  , testCase "parseMessage (incorrect format)" $
      (parseMessage "Not the right format") @?= Unknown "Not the right format"

  , testCase "parse" $
      (parse sampleLog) @?=
        [ LogMessage Info 6 "Completed armadillo processing"
        , LogMessage (Error 70) 3 "Way too many pickles"
        ]
  ]
