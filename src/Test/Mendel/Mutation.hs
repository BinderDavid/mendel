{- |
Module         : Test.Mendel.Mutation
Description    : Apply mutation operators to Haskell modules

This module provides the functionality to traverse Haskell modules and apply mutation operators.
-}
module Test.Mendel.Mutation (mutate) where

import Data.ByteString qualified as BS
import Data.Generics.Aliases (mkT)
import Data.Generics.Schemes (everywhere)
import Data.Typeable
import GHC.Data.FastString qualified as GHC
import GHC.Hs qualified as GHC
import GHC.Types.Basic qualified as GHC
import GHC.Types.Name.Occurrence qualified as GHC
import GHC.Types.Name.Reader qualified as GHC
import GHC.Types.SourceText
import GHC.Types.SrcLoc qualified as GHC
import Language.Haskell.Syntax.Expr qualified as Hs
import Language.Haskell.Syntax.Lit qualified as Hs
import Test.Mendel.MutationOperator (MuOp (..))
import Test.Mendel.MutationVariant

-------------------------------------------------------------------------------
-- Mutation on Literals
-------------------------------------------------------------------------------

reverseStringLiteral :: Hs.HsLit GHC.GhcPs -> Hs.HsLit GHC.GhcPs
reverseStringLiteral (Hs.HsString _ fs) = Hs.HsString NoSourceText (GHC.mkFastStringByteString (BS.reverse (GHC.bytesFS fs)))
reverseStringLiteral x = x

greverseStringLiteral :: forall a. (Typeable a) => a -> a
greverseStringLiteral = mkT reverseStringLiteral

-------------------------------------------------------------------------------
-- Mutation on Pattern Matches
-------------------------------------------------------------------------------

reverseClauses ::
    GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
reverseClauses (GHC.MG _ (GHC.L l body)) = GHC.MG GHC.Generated (GHC.L l (reverse body))

greverseClauses :: forall a. (Typeable a) => a -> a
greverseClauses = mkT reverseClauses

-------------------------------------------------------------------------------
-- Mutation on + and -
-------------------------------------------------------------------------------

swapPlusMinusOperator :: Hs.HsExpr GHC.GhcPs -> Hs.HsExpr GHC.GhcPs
swapPlusMinusOperator (Hs.HsVar _ (GHC.L l (GHC.Unqual v))) = Hs.HsVar GHC.NoExtField (GHC.L l (GHC.Unqual (handleOccName v)))
swapPlusMinusOperator x = x

gswapPlusMinusOperator :: forall a. (Typeable a) => a -> a
gswapPlusMinusOperator = mkT swapPlusMinusOperator

handleOccName :: GHC.OccName -> GHC.OccName
handleOccName x
    | x == GHC.mkVarOcc "+" = GHC.mkVarOcc "-"
    | x == GHC.mkVarOcc "-" = GHC.mkVarOcc "+"
    | otherwise = x

-------------------------------------------------------------------------------
-- Mutation on if
-------------------------------------------------------------------------------

swapIfElse :: Hs.HsExpr GHC.GhcPs -> Hs.HsExpr GHC.GhcPs
swapIfElse (Hs.HsIf _ i t e) = Hs.HsIf GHC.noAnn i e t
swapIfElse x = x

gswapIfElse :: forall a. (Typeable a) => a -> a
gswapIfElse = mkT swapIfElse

-------------------------------------------------------------------------------
-- Combined
-------------------------------------------------------------------------------

-- | Apply the given mutation operator to the Haskell module
mutate :: MuVariant -> GHC.HsModule GHC.GhcPs -> GHC.HsModule GHC.GhcPs
mutate ReverseString = everywhere greverseStringLiteral
mutate ReverseClausesInPatternMatch = everywhere greverseClauses
mutate SwapPlusMinus = everywhere gswapPlusMinusOperator
mutate SwapIfElse = everywhere gswapIfElse
