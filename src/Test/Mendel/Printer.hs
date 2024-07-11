{- |
Module         : Test.Mendel.Printer
Description    : Utility functions for prettyprinting objects using the GHC Api.

This module provides utility functions for prettyprinting objects which implement GHC's `Outputable` typeclass.
-}
module Test.Mendel.Printer (printOutputable, printOutputableToFile) where

import GHC.Utils.Outputable qualified as GHC
import GHC.Utils.Ppr qualified as GHC
import System.IO (IOMode (..), stdout, withFile)

mendelSDocContext :: GHC.SDocContext
mendelSDocContext = GHC.defaultSDocContext

-- | Prettyprint the given argument on stdout using sensible defaults.
printOutputable :: (GHC.Outputable p) => p -> IO ()
printOutputable p = do
    let resSDoc = GHC.ppr p
    GHC.printSDoc mendelSDocContext (GHC.PageMode True) stdout resSDoc

printOutputableToFile :: (GHC.Outputable p) => p -> FilePath -> IO ()
printOutputableToFile p fp = do
    let resSDoc = GHC.ppr p
    withFile fp WriteMode $ \handle ->
        GHC.printSDoc mendelSDocContext (GHC.PageMode True) handle resSDoc
