module Main where

import System.Environment (getArgs)
import System.Exit ( exitFailure, exitSuccess ) 
import Test.Mendel.Parser (parseModule)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fp] -> do
      parseModule fp
      exitSuccess
    _ -> exitFailure

