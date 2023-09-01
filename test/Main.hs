module Main (main) where

import GHC.Types.SrcLoc qualified as GHC
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import Test.Mendel.Parser
import Test.Mendel.Mutation
import Test.Mendel.MutationOperator
import Test.Mendel.Printer (printOutputableToFile)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [reverseStringTest]


-- | Where we find the expected result
goldenDir :: FilePath
goldenDir = "." </> "test" </> "golden"

-- | Where we put the generated output
tempDir :: FilePath
tempDir = "." </> "test" </> "temp"

-- | Where we find the candidate that we want to mutate
candidateDir :: FilePath
candidateDir = "." </> "test" </> "candidate"

reverseStringTest :: TestTree
reverseStringTest = goldenVsFile name (goldenDir </> name <.> "hs")  (tempDir </> name <.> "hs") go
  where
    name :: String
    name = "ReverseString"
    go :: IO ()
    go = do
        mmod <- parseModule (candidateDir </> name <.> "hs")
        case mmod of
            Just (GHC.L _ hmod) -> do
                let mutated = mutate ReverseString hmod
                printOutputableToFile mutated (tempDir </> name <.> "hs")
            Nothing -> pure ()

