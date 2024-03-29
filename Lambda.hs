{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Eta reduce" #-}
module Lambda where

import Data.List
import Expr
import Parser (parse_expr)

-- 1.1. find free variables of a Expr
free_vars :: Expr -> [String]
free_vars (Variable x) = [x]
free_vars (Function x expr) = free_vars expr \\ [x]
free_vars (Application e1 e2) = nub (free_vars e1 ++ free_vars e2)
free_vars (Macro x) = []

-- 1.2. reduce a redex
newVar :: String -> String
newVar x = x ++ "'"

reduce :: Expr -> String -> Expr -> Expr
reduce (Variable e1) x e2
  | e1 == x = e2
  | otherwise = Variable e1
reduce (Function e1 e) x e2
  | e1 == x = Function e1 e
  | e1 `elem` free_vars e2 = reduce (Function (newVar e1) (reduce e e1 (Variable (newVar e1)))) x e2
  | otherwise = Function e1 (reduce e x e2)
reduce (Application e1 e3) x e2 = Application (reduce e1 x e2) (reduce e3 x e2)
reduce (Macro e1) x e2 = Macro e1

-- Normal Evaluation
-- 1.3. perform one step of Normal Evaluation
stepN :: Expr -> Expr
stepN (Application (Function x e1) e2) = reduce e1 x e2
stepN (Application e1 e2)
  | e1 == stepN e1 = Application e1 (stepN e2)
  | otherwise = Application (stepN e1) e2
stepN (Function x e) = Function x (stepN e)
stepN e = e

-- 1.4. perform Normal Evaluation
reduceN :: Expr -> Expr
reduceN e
  | e == stepN e = e
  | otherwise = reduceN (stepN e)

reduceAllN :: Expr -> [Expr]
reduceAllN e
  | e == stepN e = [e]
  | otherwise = e : reduceAllN (stepN e)

-- Applicative Evaluation
-- 1.5. perform one step of Applicative Evaluation for a Expr
stepA :: Expr -> Expr
stepA (Application (Function x e1) (Variable e2)) = reduce e1 x (Variable e2)
stepA (Application (Function x e1) (Function y e2)) = reduce e1 x (Function y e2)
stepA (Application (Function x e1) e2) = Application (Function x e1) (stepA e2)
stepA (Application e1 e2)
  | e1 == stepN e1 = Application e1 (stepN e2)
  | otherwise = Application (stepN e1) e2
stepA (Function x e) = Function x (stepA e)
stepA e = e

-- 1.6. perform Applicative Evaluation
reduceA :: Expr -> Expr
reduceA e
  | e == stepA e = e
  | otherwise = reduceA (stepA e)

reduceAllA :: Expr -> [Expr]
reduceAllA e
  | e == stepA e = [e]
  | otherwise = e : reduceAllA (stepA e)

-- 3.1. make substitutions into a expression with Macros
evalMacros :: [(String, Expr)] -> Expr -> Expr
evalMacros dict expr =
  case expr of
    (Macro x) -> case lookup x dict of
      Just e -> evalMacros dict e
      Nothing -> expr
    (Application e1 e2) -> Application (evalMacros dict e1) (evalMacros dict e2)
    (Function x e) -> Function x (evalMacros dict e)
    (Variable x) -> Variable x

-- 4.1. evaluate code sequence using given strategy
evalCode :: (Expr -> Expr) -> [Code] -> [Expr]
evalCode strategy code = evalCode' [] code
  where
    evalCode' :: [(String, Expr)] -> [Code] -> [Expr]
    evalCode' dict [] = []
    evalCode' dict (c : cs) =
      case c of
        (Assign x e) -> evalCode' ((x, evalMacros dict e) : dict) cs
        (Evaluate e) -> (strategy (evalMacros dict e)) : evalCode' dict cs
