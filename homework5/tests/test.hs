import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map as M

import Calc
import ExprT
import Parser

main :: IO ()
main = defaultMain unitTests

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vars expression = expression $ M.fromList vars

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

  , testCase "Expr ExprT" $
      (mul (add (lit 2) (lit 3)) (lit 4) :: ExprT) @?=
        Mul (Add (Lit 2) (Lit 3)) (Lit 4)

  , testCase "Expr Integer" $
      (testExp :: Maybe Integer) @?= Just (-7)

  , testCase "Expr Bool" $
      (testExp :: Maybe Bool) @?= Just True

  , testCase "Expr MinMax" $
      (testExp :: Maybe MinMax) @?= Just (MinMax 5)

  , testCase "Expr Mod7" $
      (testExp :: Maybe Mod7) @?= Just (Mod7 5)

  , testCase "Expr VarExprT"  $
      (add (lit 3) (var "x") :: VarExprT) @?= Add' (Lit' 3) (Var "x")

  , testCase "var Map" $
      (withVars [("x", 6)] $ add (lit 3) (var "x")) @?= Just 9

  , testCase "var Map (blank variable)" $
      (withVars [("x", 6)] $ add (lit 3) (var "y")) @?= Nothing

  , testCase "var Map (two variables)" $
      (withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x")))
        @?= Just 54
  ]
