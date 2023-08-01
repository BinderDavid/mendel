{- |
Module         : Test.Mendel.Printer
Description    : Utility functions for prettyprinting objects using the GHC Api.

This module provides utility functions for prettyprinting objects which implement GHC's `Outputable` typeclass.
-}
module Test.Mendel.Printer (printOutputable) where

import GHC.Utils.Outputable qualified as GHC
import GHC.Utils.Ppr qualified as GHC

import System.IO ( stdout )

-- | Prettyprint the given argument on stdout using sensible defaults.
printOutputable :: GHC.Outputable p => p -> IO ()
printOutputable p = do
    let resSDoc = GHC.ppr p
    GHC.printSDoc GHC.defaultSDocContext (GHC.PageMode True) stdout resSDoc
