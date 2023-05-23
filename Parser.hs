{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use camelCase" #-}
module Parser (parse_expr, parse_code) where

import Control.Applicative
import Control.Monad
import Data.Char
import Expr

-- Parser data type
newtype Parser a = Parser
  { parse :: String -> Maybe (a, String)
  }

--- type declaration ---

instance Monad Parser where
  return val = Parser $ \str -> Just (val, str)
  (>>=) (Parser p) f = Parser $ \str ->
    case p str of
      Nothing -> Nothing
      Just (val, str') -> parse (f val) str'

instance Applicative Parser where
  pure x = return x
  pf <*> px = do
    f <- pf
    x <- px
    return $ f x

instance Functor Parser where
  fmap f px = do
    x <- px
    return $ f x

instance Alternative Parser where
  empty = fail_parser
  p1 <|> p2 =
    Parser
      ( \s -> case parse p1 s of
          Nothing -> parse p2 s
          ok -> ok
      )

--- type declaration over ---
fail_parser :: Parser a
fail_parser = Parser (const Nothing)

expr :: Parser Expr
expr = parse_application <|> parse_macro <|> parse_variable <|> parse_function

expr_atom :: Parser Expr
expr_atom = parse_application_paranthesis <|> parse_function <|> parse_macro <|> parse_variable

charParser :: Char -> Parser Char
charParser c =
  Parser
    ( \str -> case str of
        (x : xs) -> if x == c then Just (x, xs) else Nothing
        _ -> Nothing
    )

predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p =
  Parser
    ( \str -> case str of
        (x : xs) -> if p x then Just (x, xs) else Nothing
        _ -> Nothing
    )

parse_variable :: Parser Expr
parse_variable = do
  c <- predicateParser isAlpha
  return (Variable [c])

parse_function :: Parser Expr
parse_function = do
  charParser '\\'
  c <- predicateParser isAlpha
  charParser '.'
  e <- expr_atom
  return (Function [c] e)

parse_application :: Parser Expr
parse_application = do
  e1 <- expr_atom
  e2s <- many (predicateParser isSpace >> expr_atom)
  return (foldl Application e1 e2s)

parse_application_paranthesis :: Parser Expr
parse_application_paranthesis = do
  charParser '('
  e <- parse_application
  charParser ')'
  return e

parse_macro :: Parser Expr
parse_macro = do
  charParser '$'
  str <- some (predicateParser isAlpha)
  return (Macro str)

-- 2.1. parse a expression
parse_expr :: String -> Expr
parse_expr str = case parse expr str of
  Just (e, "") -> e
  _ -> error "parse error expr"

-- 4.2. parse code
code :: Parser Code
code = parse_assign <|> parse_evaluate

parse_assign :: Parser Code
parse_assign = do
  c <- some (predicateParser isAlpha)
  skip <- many (predicateParser isSpace)
  charParser '='
  skip <- many (predicateParser isSpace)
  e <- expr
  return (Assign c e)

parse_evaluate :: Parser Code
parse_evaluate = do
  e <- expr
  return (Evaluate e)

parse_code :: String -> Code
parse_code str = case parse code str of
  Just (e, "") -> e
  _ -> error "parse error code"
