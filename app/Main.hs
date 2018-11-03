module Main where

import Eval
import System.Environment
import Text.Printf
import System.Exit

start :: [String] -> IO ()
start args = do
  case eval $ head args of
    Left err -> putStrLn err >>= (\_ -> exitWith $ ExitFailure 84)
    Right result -> putStrLn $ showResult result

showResult :: (PrintfArg a, Floating a) => a -> String
showResult = printf "%0.*f" (2 :: Int)

main :: IO ()
main = start =<< getArgs
