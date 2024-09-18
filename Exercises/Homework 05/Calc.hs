{-# LANGUAGE FlexibleInstances #-}

module Calc where

import qualified Data.Map as M
import Data.Maybe
import ExprT as ET
import Parser
import StackVM as SVM

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit e) = e
eval (ET.Add x y) = eval x + eval y
eval (ET.Mul x y) = eval x * eval y

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr x = case result of
  Nothing -> Nothing
  Just e -> Just (eval e)
  where
    result = parseExp ET.Lit ET.Add ET.Mul x

-- Exercise 3
class Expr e where
  lit :: Integer -> e
  add :: e -> e -> e
  mul :: e -> e -> e

instance Expr ExprT where
  lit = ET.Lit
  add = ET.Add
  mul = ET.Mul

-- Exercise 4
instance Expr Integer where
  lit x = x
  add x y = x + y
  mul x y = x * y

instance Expr Bool where
  lit x = x > 0
  add x y = x || y
  mul x y = x && y

newtype MinMax
  = MinMax Integer
  deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = lit (max x y)
  mul (MinMax x) (MinMax y) = lit (min x y)

newtype Mod7
  = Mod7 Integer
  deriving (Eq, Show)

instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 x) (Mod7 y) = lit (x + y)
  mul (Mod7 x) (Mod7 y) = lit (x * y)

-- Exercise 5
instance Expr Program where
  lit x = [SVM.PushI x]
  add x y = x ++ y ++ [SVM.Add]
  mul x y = x ++ y ++ [SVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

-- Exercise 6
class HasVars h where
  var :: String -> h

data VarExprT = VarExprT String Integer
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = VarExprT ""
  add (VarExprT _ x) (VarExprT _ y) = lit (x + y)
  mul (VarExprT _ x) (VarExprT _ y) = lit (x * y)

instance HasVars VarExprT where
  var s = VarExprT s 0

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x = const (Just x)
  add f g m =
    if isNothing (f m) || isNothing (g m)
      then Nothing
      else Just (fromJust (f m) + fromJust (g m))
  mul f g m =
    if isNothing (f m) || isNothing (g m)
      then Nothing
      else Just (fromJust (f m) * fromJust (g m))

withVars ::
  [(String, Integer)] ->
  (M.Map String Integer -> Maybe Integer) ->
  Maybe Integer
withVars vs expr = expr $ M.fromList vs
