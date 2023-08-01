{- |
Module         : Test.Mendel.MutationOperator
Description    : Definition of mutation operators

This module provides various mutation operators which can be used
to inject faults into Haskell modules.
-}
module Test.Mendel.MutationOperator (MuOp(..)) where

-- | A mutation operator which describes a semantic change that should be applied to
-- a Haskell module.
data MuOp
  = ReverseString
  | ReverseClausesInPatternMatch
  deriving (Show, Eq)
