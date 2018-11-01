module Eval (eval, evalAst, replace) where

import Syntax
import Types
import Data.Fixed
import Data.Either

replace :: [a] -> Int -> a -> [a]
replace xs i x = (take (i - 1) xs) ++ [x] ++ (drop (i + 1) xs)

applyFunc :: ValueAst -> FuncAst -> Float
applyFunc result func = case op func of
  '+' -> (evalAst result) + (evalAst $ val func)
  '-' -> (evalAst result) - (evalAst $ val func)
  '*' -> (evalAst result) * (evalAst $ val func)
  '/' -> (evalAst result) / (evalAst $ val func)
  '%' -> (evalAst result) `mod'` (evalAst $ val func)
  '^' -> (evalAst result) ** (evalAst $ val func)
  _ -> 0

applySign :: Maybe Char -> Float -> Float
applySign sign val = case sign of
    Nothing -> val
    Just '+' -> val
    Just '-' -> (- val)
    _ -> val

processExprHead :: Maybe Char -> ValueAst -> Float
processExprHead unaryOp (Left val) = applySign unaryOp val
processExprHead unaryOp val@(Right expr) = applySign unaryOp $ evalAst val

reduceFuncs :: Char -> ExprAst -> ExprAst
reduceFuncs sym e@(ExprAst u (Left _) []) = e
reduceFuncs sym e@(ExprAst u h@(Right _) []) = ExprAst u (Left $ processExprHead u h) []
reduceFuncs sym e@(ExprAst u h (f@(FuncAst o _):fs))
  | o == sym = reduceFuncs sym $ ExprAst u (Left $ applyFunc (Left $ processExprHead u h) f) fs
  | otherwise = reduceFuncs' sym 1 e
  where
    reduceFuncs' sym i e@(ExprAst u h fs)
      | i >= length fs = e
      | op g == sym = reduceFuncs' sym i $ ExprAst u h $ (replace fs i (FuncAst (op f) (Left $ applyFunc (val f) g)))
      | otherwise = reduceFuncs' sym (i + 1) e
      where
        fn idx = fs !! idx
        g = fn i
        f = fn (i - 1)

evalAst :: ValueAst -> Float
evalAst (Left val) = val
evalAst (Right expr) = case header res of
  Left val -> val
  Right exp -> evalAst $ header exp
  where
    res = reduceFuncs '+' $ reduceFuncs '-' $ reduceFuncs '*' $ reduceFuncs '/' $ reduceFuncs '^' expr

eval :: String -> Either String Float
eval input = do
  let expr = runParser pEntry input
  case fst expr of
    Right exp -> Right $ evalAst exp
    Left err -> Left err
