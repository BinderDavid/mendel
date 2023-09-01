{- |
Module         : Test.Mendel.Mutation
Description    : Apply mutation operators to Haskell modules

This module provides the functionality to traverse Haskell modules and apply mutation operators.
-}
module Test.Mendel.Mutation (mutate) where

import GHC.Hs qualified as GHC
import Language.Haskell.Syntax.Lit qualified as Hs
import GHC.Types.SourceText
import GHC.Types.SrcLoc qualified as GHC
import GHC.Types.Basic qualified as GHC
import GHC.Data.FastString qualified as GHC

import Data.Generics.Schemes (everywhere)
import Data.Generics.Aliases (mkT)
import Data.ByteString qualified as BS
import Data.Typeable
import Test.Mendel.MutationOperator ( MuOp(..) )

-------------------------------------------------------------------------------
-- Mutation on Literals
-------------------------------------------------------------------------------

reverseStringLiteral :: Hs.HsLit GHC.GhcPs -> Hs.HsLit GHC.GhcPs
reverseStringLiteral (Hs.HsString _ fs) =  Hs.HsString NoSourceText (GHC.mkFastStringByteString (BS.reverse (GHC.bytesFS fs)))
reverseStringLiteral x = x

greverseStringLiteral :: forall a. Typeable a => a -> a
greverseStringLiteral = mkT reverseStringLiteral

-------------------------------------------------------------------------------
-- Mutation on Pattern Matches
-------------------------------------------------------------------------------

reverseClauses :: GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
reverseClauses (GHC.MG _ (GHC.L l body)) = GHC.MG GHC.Generated (GHC.L l (reverse body))

greverseClauses :: forall a. Typeable a => a -> a
greverseClauses = mkT reverseClauses

-------------------------------------------------------------------------------
-- Combined
-------------------------------------------------------------------------------

-- | Apply the given mutation operator to the Haskell module
mutate :: MuOp -> GHC.HsModule GHC.GhcPs -> GHC.HsModule GHC.GhcPs
mutate ReverseString                = everywhere greverseStringLiteral
mutate ReverseClausesInPatternMatch = everywhere greverseClauses
