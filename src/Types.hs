module Types
  ( FuncAst (..)
  , PExprAst (..)
  , ExprAst (..)
  ) where

data FuncAst = FuncAst
  { op :: Char
  , val :: ExprAst
  }

data ExprAst = ExprAst
  { head' :: Either Integer PExprAst
  , funcs :: [FuncAst]
  }

data PExprAst = PExprAst
  { sign :: Maybe Char
  , expr :: ExprAst
  }

-- let op          = exp('op', or(strongOp, weakOp));
-- let value       = exp('value', and(maybe(weakOp), number));

-- let func        = exp('func');
-- let pexpr       = exp('pexpr');
-- let expr        = exp('expr', and(or(value, pexpr), variadic(func)));
-- func.value      = and(op, expr);
-- pexpr.value     = and(maybe(weakOp), popen, expr, pclose);