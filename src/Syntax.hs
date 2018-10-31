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
  , pEntry
  , pFunc
  , pExpr
  ) where

import Parser
import Types
import Control.Applicative
import Data.Char
import Data.Maybe

pStatisfy :: (Char -> Bool) -> Parser Char
pStatisfy predicat = Parser func
  where
    func :: String -> (Either String Char, String)
    func [] = (Left "Unexpected end of string", "")
    func (x:xs) = if (predicat x) then (Right x, xs) else (Left $ "Unsatisfied char: " ++ [x], x:xs)

pNothing :: Parser (Maybe a)
pNothing = Parser func
  where
    func :: String -> (Either String (Maybe a), String)
    func xs = (Right Nothing, xs)

pIgnoreChar :: Char -> Parser Char -> Parser Char
pIgnoreChar c = (>>) $ pMany (pStatisfy (== c))

pChar :: Char -> Parser Char
pChar c = pIgnoreChar ' ' $ pStatisfy (== c)

-- One or more
pSome :: Parser a -> Parser [a]
pSome = some

-- Zéro or more
pMany :: Parser a -> Parser [a]
pMany = many

pCharOf :: String -> Parser Char
pCharOf cs = pIgnoreChar ' ' $ pStatisfy $ \c -> elem c cs

pDigit :: Parser Char
pDigit = pCharOf ['0'..'9']

pString :: String -> Parser String
pString [] = return []
pString (x:xs) = do
  x' <- pChar x
  xs' <- pString xs
  return (x':xs')

pNumber :: Parser Float
pNumber = do
  xs <- pSome pDigit
  dot <- pMaybe $ pChar '.'
  xs' <- pMany pDigit
  return $ read xs + case dot of
    Just _ -> (read xs') / (10 ^ length xs')
    Nothing -> 0.0
  -- where number xs = foldr (\x acc -> acc * 10 + x) 0 $ reverse xs

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

pValue :: Parser ValueAst
pValue = do
  openBlock <- pMaybe pOpen
  case openBlock of
    Just '(' -> do
      expr <- pExpr
      _ <- pClose
      return $ Right expr
    _ -> do
      nb <- pNumber
      return $ Left nb

pFunc :: Parser FuncAst
pFunc = do
  op <- pOp
  val <- pValue
  return $ FuncAst op val

pExpr :: Parser ExprAst
pExpr = ExprAst <$> (pMaybe pWeakOp) <*> pValue <*> (pMany pFunc)

pEntry :: Parser ValueAst
pEntry = do
  expr <- pExpr
  return $ Right expr
