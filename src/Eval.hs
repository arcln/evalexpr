module Eval (eval) where

import Syntax
import Types

applyFunc :: Integer -> FuncAst -> Integer
applyFunc result func = case op func of
  '+' -> result + (evalExpr $ val func)
  '-' -> result - (evalExpr $ val func)
  '*' -> result * (evalExpr $ val func)
  -- '/' -> result / (evalExpr $ val func)
  '%' -> result `mod` (evalExpr $ val func)
  _ -> 0

applySign :: Maybe Char -> Integer -> Integer
applySign sign val = case sign of
    Nothing -> val
    Just '+' -> val
    Just '-' -> (- val)
    _ -> val

processExprHead :: Maybe Char -> ValueAst -> Integer
processExprHead unaryOp (Left val) = applySign unaryOp val
processExprHead unaryOp val@(Right expr) = applySign unaryOp $ evalExpr val

processExpr :: ExprAst -> Integer
processExpr (ExprAst unaryOp header []) = processExprHead unaryOp header
processExpr (ExprAst unaryOp header (func:funcs)) = processExpr $ ExprAst Nothing (Left (applyFunc (processExprHead unaryOp header) func)) funcs

evalExpr :: ValueAst -> Integer
evalExpr (Left val) = val
evalExpr (Right expr) = processExpr expr

eval :: String -> Either String Integer
eval input = do
  let expr = runParser pEntry input
  case fst expr of
    Right exp -> Right $ evalExpr exp
    Left err -> Left err
