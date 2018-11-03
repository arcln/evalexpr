module Main where

import Eval
import System.Environment
import Text.Printf
import System.Exit
import System.IO

start :: [String] -> IO ()
start [] = hPutStrLn stderr "usage: ./funEvalExpr '-(2+2)*3'" >>= (\_ -> exitWith $ ExitFailure 84)
start (x:_) = do
  case eval x of
    Left err -> hPutStrLn stderr err >>= (\_ -> exitWith $ ExitFailure 84)
    Right r -> if isInfinite r
      then printResult r >>= (\_ -> exitWith $ ExitFailure 84)
      else printResult r
    where printResult r = hPutStrLn stderr $ showResult r

showResult :: Float -> String
showResult r
  | r == 0 = "0"
  | otherwise = let (_, x) = properFraction r in case x of
      0 -> printf "%0.0f" r
      -- 1 -> printf "%0.1f" r
      _ -> printf "%0.2f" r

main :: IO ()
main = start =<< getArgs
