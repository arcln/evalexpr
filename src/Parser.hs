module Parser where

newtype Parser a = Parser
  { runParser :: String -> (Either String a, String)
  }

instance Functor Parser where
  fmap = undefined

instance Applicative Parser where
  pure = returnParser
  (<*>) = sequentialParser

instance Monad Parser where
  return = returnParser
  (>>=) = bindParser

-- fmapParser :: undefined

sequentialParser :: Parser (a -> b) -> Parser a -> Parser b
sequentialParser = undefined

returnParser :: a -> Parser a
returnParser x = Parser $ \str -> (Right x, str)

bindParser :: Parser a -> (a -> Parser b) -> Parser b
bindParser x f = Parser func
  where
    func :: String -> (Either String b, String)
    func str = let (e, str1) = runParser x str
      in case e of
          Left msg -> (Left msg, str)
          Right v -> let (e', str') = runParser (f v) str'
            in case e' of
              Left msg' -> (Left msg', str)
              Right v' -> (Left "", str')

pStatisfy :: (Char -> Bool) -> Parser Char
pStatisfy predicat = Parser func
  where
    func :: String -> (Either String Char, String)
    func [] = (Left "Empty string", "")
    func (x:xs) = if (predicat x) then (Right x, xs) else (Left "Unsatisfied char", x:xs)

pChar :: Char -> Parser Char
pChar c = pStatisfy (== c)

pString :: String -> Parser String
pString [] = return []
pString (x:xs) = do
  x' <- pChar x
  xs' <- pString xs
  return (x':xs')

pBreakk :: Parser String
pBreakk = pString "break"
