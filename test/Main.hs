module Main (main) where

import GHC.Types.SrcLoc qualified as GHC
import System.FilePath
import Test.Mendel.Mutation
import Test.Mendel.MutationOperator
import Test.Mendel.MutationVariant
import Test.Mendel.Parser
import Test.Mendel.Printer (printOutputableToFile)
import Test.Tasty
import Test.Tasty.Golden

-------------------------------------------------------------------------------
-- Testsuite driver
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Tests"
        [ mkGoldenTest "ReverseString" ReverseString
        , mkGoldenTest "ReverseClausesInPatternMatch" ReverseClausesInPatternMatch
        , mkGoldenTest "SwapPlusMinus" SwapPlusMinus
        , mkGoldenTest "SwapIfElse" SwapIfElse
        ]

-------------------------------------------------------------------------------
-- Directories
-------------------------------------------------------------------------------

baseDir :: FilePath
baseDir = "./test"

-- | Where we find the expected result
goldenDir :: FilePath
goldenDir = baseDir </> "golden"

-- | Where we put the generated output
tempDir :: FilePath
tempDir = baseDir </> "temp"

-- | Where we find the candidate that we want to mutate
candidateDir :: FilePath
candidateDir = baseDir </> "candidate"

-------------------------------------------------------------------------------
-- Individual tests
-------------------------------------------------------------------------------

mkGoldenTest :: String -> MuVariant -> TestTree
mkGoldenTest name muop = goldenVsFile name (goldenDir </> name <.> "hs") (tempDir </> name <.> "hs") go
  where
    go :: IO ()
    go = do
        mmod <- parseModule (candidateDir </> name <.> "hs")
        case mmod of
            Just (GHC.L _ hmod) -> do
                let mutated = mutate' muop hmod
                printOutputableToFile mutated (tempDir </> name <.> "hs")
            Nothing -> pure ()
