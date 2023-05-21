{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Lambda where

import Data.List
import Expr

-- 1.1. find free variables of a Expr
free_vars :: Expr -> [String]
free_vars (Variable x) = [x]
free_vars (Function x expr) = free_vars expr \\ [x]
free_vars (Application e1 e2) = nub (free_vars e1 ++ free_vars e2)

-- 1.2. reduce a redex
newVar :: String -> String
newVar x = x ++ "'"

substitute :: String -> Expr -> Expr -> Expr
substitute x e2 (Variable e1) 
  | x == e1 = e2
  | otherwise = Variable e1
substitute x e2 (Function e1 e)
  | x == e1 = Function e1 e
  | e1 `elem` free_vars e2 = substitute x e2 (Function newVar_e1 (substitute e1 (Variable newVar_e1) e))
  | otherwise = Function e1 (substitute x e2 e)
  where newVar_e1 = newVar e1
substitute x e2 (Application e1 e3) = Application (substitute x e2 e1) (substitute x e2 e3)

reduce :: Expr -> String -> Expr -> Expr
reduce e1 x e2 = substitute x e2 e1

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
