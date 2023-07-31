module Main where

import System.Environment (getArgs)
import System.Exit ( exitFailure, exitSuccess ) 
import Test.Mendel.Parser (parseModule)
import Test.Mendel.Printer (printOutputable)
import Test.Mendel.Mutation (mutate)
import GHC.Types.SrcLoc qualified as GHC

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fp] -> do
      GHC.L _ mod <- parseModule fp
      putStrLn "-------------------------------------------------------"
      putStrLn "BEFORE"
      putStrLn "-------------------------------------------------------"
      printOutputable mod
      let mutant = mutate mod
      putStrLn "\n-------------------------------------------------------"
      putStrLn "AFTER"
      putStrLn "-------------------------------------------------------"
      printOutputable mutant
      exitSuccess
    _ -> exitFailure

