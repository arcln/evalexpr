module Syntax
    ( runParser
    , pStatisfy
    , pChar
    , pSome
    , pMany
    , pCharOf
    , pString
    , pNumber
    , pBreak
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

pChar :: Char -> Parser Char
pChar c = pStatisfy (== c)

-- One or more
pSome :: Parser a -> Parser [a]
pSome x = (:) <$> x <*> (pMany x)

-- Zéro or more
pMany :: Parser a -> Parser [a]
pMany x = (pSome x) <|> pure []

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
  
pBreak :: Parser String
pBreak = (pString "break") <|> (pString ";")

-- let number      = exp('number', /[0-9]*/);
-- let strongOp    = exp('strongOp', /[*|/]/);
-- let weakOp      = exp('weakOp', /\+|-/);

-- let popen       = exp('popen', /\(/);
-- let pclose      = exp('pclose', /\)/);

-- let op          = exp('op', or(strongOp, weakOp));
-- let value       = exp('value', and(maybe(weakOp), number));

-- let func        = exp('func');
-- let pexpr       = exp('pexpr');
-- let expr        = exp('expr', and(or(value, pexpr), variadic(func)));
-- func.value      = and(op, expr);
-- pexpr.value     = and(maybe(weakOp), popen, expr, pclose);
