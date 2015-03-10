module Calc where

import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

evalStr :: String -> Maybe Integer
evalStr = maybeEval . parseExp Lit Add Mul
  where
    maybeEval (Just x) = Just $ eval x
    maybeEval _ = Nothing
