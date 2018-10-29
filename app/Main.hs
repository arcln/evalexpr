module Main where

import Syntax

main :: IO ()
main = do
    let (a, b) = runParser (pChar 'b') "break"
    print $ case a of
        Left a' -> a'
        Right a' -> a' : ('-' : b)

    let (a2, b2) = runParser pBreak ";break"
    print $ case a2 of
        Left a2' -> a2'
        Right a2' -> a2' ++ " - " ++ b2

    let (a3, b3) = runParser pNumber "142sdf"
    print $ case a3 of
        Left a3' -> a3'
        Right a3' -> (show a3') ++ " - " ++ b3
        