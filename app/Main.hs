module Main where

import Eval
import System.Environment

start :: [String] -> IO ()
start args = print $ eval $ head args

main :: IO ()
main = start =<< getArgs

    -- let (a3, b3) = runParser pNumber "-142sdf"
    -- print $ case a3 of
    --     Left a3' -> a3'
    --     Right a3' -> (show a3') ++ " - " ++ b3

    -- let (a3, b3) = runParser pValue "-3456"
    -- print $ case a3 of
    --     Left a3' -> a3'
    --     Right a3' -> (show a3') ++ " - " ++ b3
