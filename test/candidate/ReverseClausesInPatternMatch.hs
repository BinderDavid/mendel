module ReverseClausesInPatternMatch where

reverseMe :: Bool -> Bool
reverseMe x = case x of
    True -> False
    False -> True
