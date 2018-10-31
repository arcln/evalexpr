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
