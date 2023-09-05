module ReverseClausesInPatternMatch where
reverseMe :: Bool -> Bool
reverseMe x
  = case x of
      False -> True
      True -> False