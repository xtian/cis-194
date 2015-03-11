{-# LANGUAGE FlexibleInstances #-}

module Calc where

import Control.Applicative (liftA2, (<$>))
import qualified Data.Map as M

import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

evalStr :: String -> Maybe Integer
evalStr x = eval <$> parseExp Lit Add Mul x

class Expr a where
  lit :: Integer -> a
  add, mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)


newtype MinMax = MinMax Integer
  deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax $ max x y
  mul (MinMax x) (MinMax y) = MinMax $ min x y


newtype Mod7 = Mod7 Integer
  deriving (Eq, Show)

instance Expr Mod7 where
  lit = Mod7 . max 0 . min 6
  add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7


data VarExprT = Lit' Integer
              | Add' VarExprT VarExprT
              | Mul' VarExprT VarExprT
              | Var String
  deriving (Show, Eq)

class HasVars a where
  var :: String -> a

instance HasVars VarExprT where
  var = Var

instance Expr VarExprT where
  lit = Lit'
  add = Add'
  mul = Mul'

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var key vars = M.lookup key vars

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x _ = Just x
  add x y vars = liftA2 (+) (x vars) (y vars)
  mul x y vars = liftA2 (*) (x vars) (y vars)
