module Parser where

import Control.Applicative

newtype Parser a = Parser
  { runParser :: String -> (Either String a, String)
  }

instance Functor Parser where
  fmap = fmapParser

instance Applicative Parser where
  pure = returnParser
  (<*>) = sequentialParser -- Not sure about the name, if you know better, change it

instance Alternative Parser where
  empty = emptyParser
  (<|>) = pipeParser -- Not sure about the name, if you know better, change it

instance Monad Parser where
  return = returnParser
  (>>=) = bindParser

fmapParser :: (a -> b) -> Parser a -> Parser b
fmapParser f x = do
  x' <- x
  return $ f x'

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
