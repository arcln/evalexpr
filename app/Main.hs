module Main where

import Parser

main :: IO ()
main = do
    let (a, b) = runParser (pChar 'b') "break"
    print $ case a of
        Left a' -> a'
        Right a' -> a' : ('-' : b)

    let (a', b') = runParser (pString "br") "break"
    print a'
    print b'
