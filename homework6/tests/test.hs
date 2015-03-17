import Test.Tasty
import Test.Tasty.HUnit

import Fibonacci

main :: IO ()
main = defaultMain unitTests

streamTake :: Int -> Stream a -> [a]
streamTake n = take n . streamToList


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

  , testCase "streamRepeat" $
      (streamTake 4 $ streamRepeat 0) @?= ([0, 0, 0, 0] :: [Integer])

  , testCase "streamMap" $
      (streamTake 2 $ streamMap (+1) $ streamRepeat 0) @?= ([1, 1] :: [Integer])

  , testCase "streamFromSeed" $
      (streamTake 3 $ streamFromSeed (+1) 0) @?= ([0, 1, 2] :: [Integer])

  , testCase "nats" $
      (streamTake 3 $ nats) @?= [0, 1, 2]

  , testCase "interleaveStreams" $
      (streamTake 4 $ interleaveStreams (streamRepeat 0) (streamRepeat 1)) @?= ([0, 1, 0, 1] :: [Integer])

  , testCase "ruler" $
      (streamTake 16 $ ruler) @?= [0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4]
  ]
