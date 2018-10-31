module Main where

import Eval
import System.Environment

start :: [String] -> IO ()
start args = do
  case eval $ head args of
    Left err -> print err
    Right result -> print result

main :: IO ()
main = start =<< getArgs
