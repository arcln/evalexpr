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

-- pAnd :: Parser a -> Parser b -> Parser c
-- pAnd x y = do
--   x' <- x
--   y' <- y

pValue :: Parser Integer
pValue = do
  x <- pMaybe pWeakOp
  num <- pNumber
  case x of
    Just '-' -> return (- num)
    Just '+' -> return num
    Nothing -> pNumber

-- let op          = exp('op', or(strongOp, weakOp));
-- let value       = exp('value', and(maybe(weakOp), number));

-- let func        = exp('func');
-- let pexpr       = exp('pexpr');
-- let expr        = exp('expr', and(or(value, pexpr), variadic(func)));
-- func.value      = and(op, expr);
-- pexpr.value     = and(maybe(weakOp), popen, expr, pclose);
