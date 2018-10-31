module Types
  ( FuncAst (..)
  , ExprAst (..)
  , ValueAst (..)
  ) where

type ValueAst = Either Integer ExprAst

data FuncAst = FuncAst
  { op :: Char
  , val :: ValueAst
  } deriving (Show)

data ExprAst = ExprAst
  { unaryOp :: Maybe Char
  , header :: ValueAst
  , funcs :: [FuncAst]
  } deriving (Show)

-- let op          = exp('op', or(strongOp, weakOp));
-- let value       = exp('value', and(maybe(weakOp), number));

-- let func        = exp('func');
-- let pexpr       = exp('pexpr');
-- let expr        = exp('expr', and(or(value, pexpr), variadic(func)));
-- func.value      = and(op, expr);
-- pexpr.value     = and(maybe(weakOp), popen, expr, pclose);