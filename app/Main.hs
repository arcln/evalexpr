module Main where

import Syntax

main :: IO ()
main = do
    let (a, b) = runParser (pChar 'b') "break"
    print $ case a of
        Left a' -> a'
        Right a' -> a' : ('-' : b)

    let (a3, b3) = runParser pNumber "-142sdf"
    print $ case a3 of
        Left a3' -> a3'
        Right a3' -> (show a3') ++ " - " ++ b3

    let (a3, b3) = runParser pValue "-3456"
    print $ case a3 of
        Left a3' -> a3'
        Right a3' -> (show a3') ++ " - " ++ b3
