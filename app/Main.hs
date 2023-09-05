module Main where

import System.Exit ( exitFailure, exitSuccess ) 
import Test.Mendel.Parser (parseModule)
import Test.Mendel.Printer (printOutputable)
import Test.Mendel.Mutation (mutate)
import GHC.Types.SrcLoc qualified as GHC

import Options ( Options(..), parseOptions )
import Data.Version (showVersion)

import Paths_mendel (version)

main :: IO ()
main = do
  opts <- parseOptions
  dispatch opts

printVersion :: IO ()
printVersion = putStrLn ("Version: " <> showVersion version)

dispatch :: Options -> IO ()
dispatch (MutateFile mo fp) = do
  mmod <- parseModule fp
  case mmod of
    Just (GHC.L _ hmod) -> do
      putStrLn "-------------------------------------------------------"
      putStrLn "BEFORE"
      putStrLn "-------------------------------------------------------"
      printOutputable hmod
      let mutant = mutate mo hmod
      putStrLn "\n-------------------------------------------------------"
      putStrLn "AFTER"
      putStrLn "-------------------------------------------------------"
      printOutputable mutant
      putStrLn ""
      exitSuccess
    Nothing -> exitFailure
dispatch Version = printVersion
