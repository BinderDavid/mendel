{- |
Module         : Test.Mendel.Mutation
Description    : Apply mutation operators to Haskell modules

This module provides the functionality to traverse Haskell modules and apply mutation operators.
-}
module Test.Mendel.Mutation (mutate) where

import GHC.Hs qualified as GHC
import Language.Haskell.Syntax.Lit qualified as Hs
import GHC.Types.SourceText

import Data.Generics.Schemes (everywhere)
import Data.Generics.Aliases (mkT)
import Data.Typeable
import Data.String
import Test.Mendel.MutationOperator ( MuOp(..) )

reverseStringLiteral :: Hs.HsLit GHC.GhcPs -> Hs.HsLit GHC.GhcPs
reverseStringLiteral (Hs.HsString _ fs) =  Hs.HsString NoSourceText (fromString  (reverse (show fs)))
reverseStringLiteral x = x

greverseStringLiteral :: forall a. Typeable a => a -> a
greverseStringLiteral = mkT reverseStringLiteral

-- | Apply the given mutation operator to the Haskell module
mutate :: MuOp -> GHC.HsModule GHC.GhcPs -> GHC.HsModule GHC.GhcPs
mutate ReverseString = everywhere greverseStringLiteral
