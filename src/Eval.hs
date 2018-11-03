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

processExprHead :: ExprAst -> ExprAst
processExprHead e@(ExprAst unaryOp (Left val) fs) = ExprAst Nothing (Left $ applySign unaryOp val) fs
processExprHead e@(ExprAst unaryOp val@(Right _) fs) = ExprAst Nothing (Left $ applySign unaryOp $ evalAst val) fs

reduceFuncs :: Char -> ExprAst -> ExprAst
reduceFuncs sym e@(ExprAst u (Left _) []) = e
reduceFuncs sym e@(ExprAst u h@(Right _) []) = processExprHead $ ExprAst u h []
reduceFuncs sym e@(ExprAst u h (f@(FuncAst o _):fs))
  | o == sym = reduceFuncs sym $ ExprAst Nothing (Left $ applyFunc (header $ processExprHead expr') f) fs
  | otherwise = reduceFuncs' sym 1 e
  where
    expr' = ExprAst u h []
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
  Left val -> applySign (unaryOp res) val
  Right exp -> evalAst $ header exp
  where
    res = foldr reduceFuncs expr ['^', '%', '/', '*', '-', '+']

eval :: String -> Either String Float
eval input = do
  let expr = runParser pEntry input
  if length (snd expr) /= 0
    then Left $ "Unexpected token: " ++ (snd expr)
    else case fst expr of
      Right exp -> Right $ evalAst exp
      Left err -> Left err
