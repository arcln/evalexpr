module Syntax
  ( runParser
  , pStatisfy
  , pChar
  , pSome
  , pMany
  , pCharOf
  , pString
  , pNumber
  , pValue
  ) where

import Parser
import Types
import Control.Applicative
import Data.Char

pStatisfy :: (Char -> Bool) -> Parser Char
pStatisfy predicat = Parser func
  where
    func :: String -> (Either String Char, String)
    func [] = (Left "Empty string", "")
    func (x:xs) = if (predicat x) then (Right x, xs) else (Left "Unsatisfied char", x:xs)

pNothing :: Parser (Maybe a)
pNothing = Parser func
  where
    func :: String -> (Either String (Maybe a), String)
    func xs = (Right Nothing, xs)

pChar :: Char -> Parser Char
pChar c = pStatisfy (== c)

-- pOr :: Parser a -> Parser b -> Parser c
-- pOr x y = do
--   x' <- x
--   y' <- y
--   case y' of

-- One or more
pSome :: Parser a -> Parser [a]
pSome = some

-- Zéro or more
pMany :: Parser a -> Parser [a]
pMany = many

pCharOf :: String -> Parser Char
pCharOf cs = pStatisfy $ \c -> elem c cs

pDigit :: Parser Integer
pDigit = toInteger . digitToInt <$> pCharOf ['0'..'9']

pString :: String -> Parser String
pString [] = return []
pString (x:xs) = do
  x' <- pChar x
  xs' <- pString xs
  return (x':xs')

pNumber :: Parser Integer
pNumber = do
  x <- pDigit
  xs <- pNumber <|> pure 0
  return (x + xs * 10)

pStrongOp :: Parser Char
pStrongOp = pChar '*' <|> pChar '/'

pWeakOp :: Parser Char
pWeakOp = pChar '+' <|> pChar '-'

pOpen :: Parser Char
pOpen = pChar '('

pClose :: Parser Char
pClose = pChar ')'

pOp :: Parser Char
pOp = pWeakOp <|> pStrongOp

pMaybe :: Parser a -> Parser (Maybe a)
pMaybe = optional

pValue :: Parser Integer
pValue = do
  sign <- pMaybe pWeakOp
  num <- pNumber
  case sign of
    Just '-' -> return (- num)
    Just '+' -> return num
    Nothing -> pNumber

pFunc :: Parser FuncAst
pFunc = do
  op <- pOp
  expr <- pExpr
  return $ FuncAst op expr

pExpr :: Parser ExprAst
pExpr = do
  header <- pOr pValue pPExpr
  funcs <- pMany pFunc 
  return $ ExprAst header funcs

pPExpr :: Parser PExprAst
pPExpr = do
  sign <- pMaybe pWeakOp
  expr <- (pChar '(') *> pExpr (<* pChar ')')
  return $ PExprAst sign expr


-- let pexpr       = exp('pexpr');
-- let expr        = exp('expr', and(or(value, pexpr), variadic(func)));
-- func.value      = and(op, expr);
-- pexpr.value     = and(maybe(weakOp), popen, expr, pclose);
