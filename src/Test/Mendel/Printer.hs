module Test.Mendel.Printer (printOutputable) where

import GHC.Utils.Outputable qualified as GHC
import GHC.Utils.Ppr qualified as GHC

import System.IO ( stdout )

printOutputable :: GHC.Outputable p => p -> IO ()
printOutputable p = do
    putStrLn "Parse succeeded"
    let resSDoc = GHC.ppr p
    GHC.printSDoc GHC.defaultSDocContext (GHC.PageMode True) stdout resSDoc
    pure ()