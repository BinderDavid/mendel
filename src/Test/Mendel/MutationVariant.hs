{- |
Module         : Test.Mendel.MutationVariant
Description    : Definition of mutation operators

This module provides various mutation operators which can be used
to inject faults into Haskell modules.
-}
module Test.Mendel.MutationVariant where

{- | A mutation operator which describes a semantic change that should be applied to
a Haskell module.
-}
data MuVariant
    = MutatePatternMatch
    | MutateValues
    | MutateFunctions
    | MutateNegateIfElse
    | MutateNegateGuards
    | MutateOther String
    | ReverseString
    | ReverseClausesInPatternMatch
    | SwapPlusMinus
    | SwapIfElse
    deriving (Show, Eq)
