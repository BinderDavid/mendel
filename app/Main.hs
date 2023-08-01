module Main where

import System.Exit ( exitFailure, exitSuccess ) 
import Test.Mendel.Parser (parseModule)
import Test.Mendel.Printer (printOutputable)
import Test.Mendel.Mutation (mutate)
import Test.Mendel.MutationOperator
import GHC.Types.SrcLoc qualified as GHC

import Options
import Options.Applicative

main :: IO ()
main = do
  opts <- execParser optionsParser'
  dispatch opts


dispatch :: Options -> IO ()
dispatch (MutateFile fp) = do
  mmod <- parseModule fp
  case mmod of
    Just (GHC.L _ hmod) -> do
      putStrLn "-------------------------------------------------------"
      putStrLn "BEFORE"
      putStrLn "-------------------------------------------------------"
      printOutputable hmod
      let mutant = mutate ReverseString hmod
      putStrLn "\n-------------------------------------------------------"
      putStrLn "AFTER"
      putStrLn "-------------------------------------------------------"
      printOutputable mutant
      putStrLn ""
      exitSuccess
    Nothing -> exitFailure
