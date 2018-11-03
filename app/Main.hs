module Main where

import Eval
import System.Environment
import Text.Printf
import System.Exit

start :: [String] -> IO ()
start [] = putStrLn "usage: ./funEvalExpr '-(2+2)*3'" >>= (\_ -> exitWith $ ExitFailure 84)
start (x:_) = do
  case eval x of
    Left err -> putStrLn err >>= (\_ -> exitWith $ ExitFailure 84)
    Right result -> putStrLn $ showResult result

showResult :: (PrintfArg a, Floating a) => a -> String
showResult = printf "%0.*f" (2 :: Int)

main :: IO ()
main = start =<< getArgs
