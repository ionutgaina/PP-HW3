{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Lambda where

import Data.List
import Expr

-- 1.1. find free variables of a Expr
free_vars :: Expr -> [String]
free_vars (Variable v) = [v]
free_vars (Function v expr) = free_vars expr \\ [v]
free_vars (Application e1 e2) = nub (free_vars e1 ++ free_vars e2)

-- 1.2. reduce a redex
newVar :: String -> String
newVar x = x ++ "'"

substitute :: String -> Expr -> Expr -> Expr
substitute x e' (Variable y)
  | x == y = e'
  | otherwise = Variable y
substitute x e' (Function y e)
  | x == y = Function y e
  | y `elem` free_vars e' = let y' = newVar y in Function y' (substitute x e' (substitute y (Variable y') e))
  | otherwise = Function y (substitute x e' e)
substitute x e' (Application e1 e2) = Application (substitute x e' e1) (substitute x e' e2)

reduce :: Expr -> String -> Expr -> Expr
reduce e x e' = substitute x e' e

-- Normal Evaluation
-- 1.3. perform one step of Normal Evaluation
stepN :: Expr -> Expr
stepN (Application (Function x e1) e2) = reduce e1 x e2
stepN (Application e1 e2) = Application (stepN e1) e2
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
-- TODO 1.5. perform one step of Applicative Evaluation
stepA :: Expr -> Expr
stepA = undefined

-- TODO 1.6. perform Applicative Evaluation
reduceA :: Expr -> Expr
reduceA e 
  | e == stepA e = e
  | otherwise = reduceA (stepA e)

reduceAllA :: Expr -> [Expr]
reduceAllA e 
  | e == stepA e = [e]
  | otherwise = e : reduceAllA (stepA e)
  
-- TODO 3.1. make substitutions into a expression with Macros
evalMacros :: [(String, Expr)] -> Expr -> Expr
evalMacros = undefined

-- TODO 4.1. evaluate code sequence using given strategy
evalCode :: (Expr -> Expr) -> [Code] -> [Expr]
evalCode = undefined
