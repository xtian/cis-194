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

  , testCase "foldTree" $
      (foldTree "ABCDEFGHIJ") @?=
        Node 3
          (Node 2
            (Node 0 Leaf 'F' Leaf)
            'I'
            (Node 1 (Node 0 Leaf 'B' Leaf) 'C' Leaf)
          )
          'J'
          (Node 2
            (Node 1 (Node 0 Leaf 'A' Leaf) 'G' Leaf)
            'H'
            (Node 1 (Node 0 Leaf 'D' Leaf) 'E' Leaf)
          )

  , testCase "xor n=3" $
      (xor [False, True, False]) @?= True

  , testCase "xor n=5" $
      (xor [False, True, False, False, True]) @?= False
  ]
