module Eval (eval) where

import Syntax
import Types
import Data.Fixed

applyFunc :: Float -> FuncAst -> Float
applyFunc result func = case op func of
  '+' -> result + (evalExpr $ val func)
  '-' -> result - (evalExpr $ val func)
  '*' -> result * (evalExpr $ val func)
  '/' -> result / (evalExpr $ val func)
  '%' -> result `mod'` (evalExpr $ val func)
  _ -> 0

applySign :: Maybe Char -> Float -> Float
applySign sign val = case sign of
    Nothing -> val
    Just '+' -> val
    Just '-' -> (- val)
    _ -> val

processExprHead :: Maybe Char -> ValueAst -> Float
processExprHead unaryOp (Left val) = applySign unaryOp val
processExprHead unaryOp val@(Right expr) = applySign unaryOp $ evalExpr val

processExpr :: ExprAst -> Float
processExpr (ExprAst unaryOp header []) = processExprHead unaryOp header
processExpr (ExprAst unaryOp header (func:funcs)) = processExpr $ ExprAst Nothing (Left (applyFunc (processExprHead unaryOp header) func)) funcs

evalExpr :: ValueAst -> Float
evalExpr (Left val) = val
evalExpr (Right expr) = processExpr expr

eval :: String -> Either String Float
eval input = do
  let expr = runParser pEntry input
  case fst expr of
    Right exp -> Right $ evalExpr exp
    Left err -> Left err
