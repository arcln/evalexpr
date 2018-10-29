module Parser where

import Data.Char
import Control.Applicative

newtype Parser a = Parser
  { runParser :: String -> (Either String a, String)
  }

instance Functor Parser where
  fmap = fmapParser

instance Applicative Parser where
  pure = returnParser
  (<*>) = sequentialParser

instance Alternative Parser where
  empty = emptyParser
  (<|>) = pipeParser

instance Monad Parser where
  return = returnParser
  (>>=) = bindParser

-- fmapParser :: undefined

sequentialParser :: Parser (a -> b) -> Parser a -> Parser b
sequentialParser f x = do
  f' <- f
  x' <- x
  return $ f' x'

emptyParser :: Parser a
emptyParser = Parser $ \x -> (Left "", x)

pipeParser :: Parser a -> Parser a -> Parser a
pipeParser x x' = Parser $ \str -> let (e, str') = runParser x str
  in case e of
    Left msg -> runParser x' str
    Right v -> (Right v, str')

returnParser :: a -> Parser a
returnParser x = Parser $ \str -> (Right x, str)

bindParser :: Parser a -> (a -> Parser b) -> Parser b
bindParser x f = Parser $ \str -> let (e, str1) = runParser x str
  in case e of
    Left msg -> (Left msg, str)
    Right v -> let (e', str2) = runParser (f v) str1
      in case e' of
        Left msg' -> (Left msg', str)
        Right v' -> (Right v', str2)

pStatisfy :: (Char -> Bool) -> Parser Char
pStatisfy predicat = Parser func
  where
    func :: String -> (Either String Char, String)
    func [] = (Left "Empty string", "")
    func (x:xs) = if (predicat x) then (Right x, xs) else (Left "Unsatisfied char", x:xs)

pChar :: Char -> Parser Char
pChar c = pStatisfy (== c)

pMany :: Parser a -> Parser [a]
pMany x = many_x
  where
    many_x = some_x <|> pure []
    some_x = (:) <$> x <*> many_x

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
  x <- pMany pDigit
  return $ foldr (\x y -> y * 10 + x) 0 x

-- pNumber :: Parser Integer
-- pNumber = do
--     x <- pDigit
--     xs <- pNumber
--     return $ (toInteger $ digitToInt x) * 10 + xs

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