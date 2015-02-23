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
  , "I 4 Everything normal"
  ]

messageAt :: Int -> LogMessage
messageAt t = LogMessage Info t (show t)

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
        , LogMessage Info 4 "Everything normal"
        ]

  , testCase "insert (unknown)" $
      (insert (Unknown "foo") Leaf) @?= Leaf

  , testCase "insert (lt)" $
      (insert (messageAt 1) (Node Leaf (messageAt 2) Leaf)) @?=
        Node
          (Node Leaf (messageAt 1) Leaf)
          (messageAt 2)
          Leaf

  , testCase "insert (gt)" $
      (insert (messageAt 2) (Node Leaf (messageAt 1) Leaf)) @?=
        Node
          Leaf
          (messageAt 1)
          (Node Leaf (messageAt 2) Leaf)

  , testCase "build" $
      (build $ parse sampleLog) @?=
        Node
          (Node
            Leaf
              (LogMessage (Error 70) 3 "Way too many pickles")
            (Node
              Leaf
                (LogMessage Info 4 "Everything normal")
              Leaf)
            )
            (LogMessage Info 6 "Completed armadillo processing")
          Leaf

  , testCase "inOrder" $
      (inOrder $ build $ parse sampleLog) @?=
        [ LogMessage (Error 70) 3 "Way too many pickles"
        , LogMessage Info 4 "Everything normal"
        , LogMessage Info 6 "Completed armadillo processing"
        ]
  ]
