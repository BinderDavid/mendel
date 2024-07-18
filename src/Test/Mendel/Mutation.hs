{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

{- |
Module         : Test.Mendel.Mutation
Description    : Apply mutation operators to Haskell modules

This module provides the functionality to traverse Haskell modules and apply mutation operators.
-}

{-# LANGUAGE  TupleSections, RankNTypes #-}

module Test.Mendel.Mutation (programMutants, 
                             selectLitOps,
                             --selectBLitOps,
                             selectIfElseBoolNegOps,
                             --selectGuardedBoolNegOps,
                             --selectFnMatches,
                             mutate'
                             ) where

module Test.Mendel.Mutation (
    -- programMutants,
    selectLitOps,
    -- selectBLitOps,
    selectIfElseBoolNegOps,
    -- selectGuardedBoolNegOps,
    -- selectFnMatches,
    mutate',
) where

-- GuardedRhs_,

import Control.Monad (MonadPlus, mplus)
import Data.ByteString qualified as BS
import Data.Generics (Data, GenericM, Typeable, gmapMo, listify, mkMp)
import Data.Generics.Aliases (mkT)
import Data.Generics.Schemes (everywhere)
import Data.List (nub, permutations, subsequences, (\\))
import Data.Maybe (isJust)
import Data.Typeable
import GHC.Data.FastString qualified as GHC
import GHC.Hs
import Language.Haskell.Syntax.Lit
import Language.Haskell.Syntax.Expr
import GHC.Types.SourceText
import GHC.Types.SrcLoc qualified as GHC
import Language.Haskell.Syntax.Expr
import Language.Haskell.Syntax.Lit
import Test.Mendel.Config (
    Config (muOp),
    FnOp (_fns, _type),
    FnType (FnIdent, FnSymbol),
 )
import Test.Mendel.MutationOperator (
    Decl_ (..),
    Exp_ (..),
    Module_,
    MuOp,
    Mutable,
    Wrapped (..),
    getSpan,
    mkMpMuOp,
    same,
    (==>*),
 )
import Test.Mendel.MutationVariant

type Span = (Int, Int, Int, Int)

{- | apply a mutating function on a piece of code one at a time
like somewhere (from so)
-}
once :: (MonadPlus m) => GenericM m -> GenericM m
once f x = f x `mplus` gmapMo (once f) x

{- | The function `relevantOps` does two filters. For the first, it
removes spurious transformations like "Int 1 ~~> Int 1". Secondly, it
tries to apply the transformation to the given program on some element
if it does not succeed, then we discard that transformation.
-}
relevantOps :: (Data a, Eq a) => a -> [(MuVariant, MuOp)] -> [(MuVariant, MuOp)]
relevantOps m oplst = filter (relevantOp m) $ filter (not . same . snd) oplst
  where
    -- check if an operator can be applied to a program
    relevantOp m' (_v, op) = isJust $ once (mkMpMuOp op) m'

{- | convert a tuple with element and second array to an array of
tuples by repeating the first element
-}
spread :: (a, [b]) -> [(a, b)]
spread (a, lst) = map (a,) lst

-- | The `choose` function generates subsets of a given size
choose :: [a] -> Int -> [[a]]
choose xs n = filter (\x -> length x == n) $ subsequences xs

{- | Produce all mutants after applying all operators
programMutants ::
     Config                   -- ^ Configuration
  -> Module_                  -- ^ Module to mutate
  -> [(MuVariant, Span, Module_)] -- ^ Returns mutated modules
programMutants config ast =  nub $ mutatesN (applicableOps config ast) ast fstOrder
  where fstOrder = 1 -- first order
-}

{- | First and higher order mutation. The actual apply of mutation operators,
and generation of mutants happens here.
The third argument specifies whether it's first order or higher order
-}
mutatesN ::
    -- | Applicable Operators
    [(MuVariant, MuOp)] ->
    -- | Module to mutate
    Module_ ->
    -- | Order of mutation (usually 1 - first order)
    Int ->
    -- | Returns the mutated module
    [(MuVariant, Span, Module_)]
mutatesN os ast = mutatesN' os (MutateOther [], (0, 0, 0, 0), ast)
  where
    mutatesN' ops ms 1 = concat [mutate op ms | op <- ops]
    mutatesN' ops ms c = concat [mutatesN' ops m 1 | m <- mutatesN' ops ms $ pred c]

{- | Given a function, generate all mutants after applying applying
op once (op might be applied at different places).
E.g.: if the operator is (op = "<" ==> ">") and there are two instances of
"<" in the AST, then it will return two AST with each replaced.
-}
mutate :: (MuVariant, MuOp) -> (MuVariant, Span, Module_) -> [(MuVariant, Span, Module_)]
mutate (v, op) (_v, _s, m) = map (v,getSpan op,) $ once (mkMpMuOp op) m \\ [m]

{- | Returns all mutation operators
applicableOps ::
     Config                   -- ^ Configuration
  -> Module_                  -- ^ Module to mutate
  -> [(MuVariant,MuOp)]           -- ^ Returns mutation operators
applicableOps config ast = relevantOps ast opsList
  where opsList = concatMap spread [
            (MutatePatternMatch, selectFnMatches ast),
            (MutateValues, selectLiteralOps ast),
            (MutateFunctions, selectFunctionOps (muOp config) ast),
            (MutateNegateIfElse, selectIfElseBoolNegOps ast)]
(MutateNegateGuards, selectGuardedBoolNegOps ast)
-}

{- | For valops, we specify how any given literal value might
change. So we take a predicate specifying how to recognize the literal
value, a list of mappings specifying how the literal can change, and the
AST, and recurse over the AST looking for literals that match our predicate.
When we find any, we apply the given list of mappings to them, and produce
a MuOp mapping between the original value and transformed value. This list
of MuOp mappings are then returned.
-}
selectValOps ::
    (Typeable a, Typeable b, Mutable b) => (a -> Bool) -> (b -> [b]) -> Module_ -> [MuOp]
selectValOps predicate f m = concat [x ==>* f x | x <- vals]
  where
    vals = map wrap $ listify predicate m
    wrap = undefined

-- | Look for literal values in AST, and return applicable MuOp transforms.
selectLiteralOps :: Module_ -> [MuOp]
selectLiteralOps m = selectLitOps m ++ selectBLitOps m

{- | Look for literal values in AST, and return applicable MuOp transforms.
Unfortunately booleans are not handled here.
-}
selectLitOps :: Module_ -> [MuOp]
selectLitOps = undefined -- selectValOps isLit convert
  where
    isLit :: LHsExpr GhcPs -> Bool
    isLit (GHC.L _ (HsLit _ (HsInt{}))) = True
    isLit (GHC.L _ (HsLit _ (HsIntPrim{}))) = True
    isLit (GHC.L _ (HsLit _ (HsChar{}))) = True
    isLit (GHC.L _ (HsLit _ (HsCharPrim{}))) = True
    isLit (GHC.L _ (HsLit _ (HsFloatPrim{}))) = True
    isLit (GHC.L _ (HsLit _ (HsDoublePrim{}))) = True
    isLit (GHC.L _ (HsLit _ (HsString{}))) = True
    isLit (GHC.L _ (HsLit _ (HsStringPrim{}))) = True
    isLit (GHC.L _ (HsLit _ (HsWordPrim{}))) = True
    isLit (GHC.L _ (HsLit _ (HsInteger{}))) = True
    isLit (GHC.L _ (HsLit _ (HsRat{}))) = True
    isLit (GHC.L _ (HsLit _ (HsInt64Prim{}))) = True
    isLit (GHC.L _ (HsLit _ (HsWord64Prim{}))) = True
    isLit _ = False
    -- convert (HsInt l i) = map (apX (HsInt l i)) $ nub [i + 1, i - 1, 0, 1]
    -- convert (HsIntPrim l i) = map (apX (HsIntPrim l i)) $ nub [i + 1, i - 1, 0, 1]
    -- convert (HsChar l c) = map (apX (HsChar l)) [pred c, succ c]
    -- convert (HsCharPrim l c) = map (apX (HsCharPrim l)) [pred c, succ c]
    -- convert (HsFloatPrim l f) = map (apX (HsFloatPrim l)) $ nub [f + 1.0, f - 1.0, 0.0, 1.0]
    -- convert (HsDoublePrim l f) = map (apX (HsDoublePrim l)) $ nub [f + 1.0, f - 1.0, 0.0, 1.0]
    -- convert (HsString l _) = map (apX (HsString l)) $ nub [""]
    -- convert (HsStringPrim l _) = map (apX (HsStringPrim l)) $ nub [""]
    -- convert (HsWordPrim l i) = map (apX (HsWordPrim l)) $ nub [i + 1, i - 1, 0, 1]
    apX :: (t1 -> [a] -> t) -> t1 -> t
    apX fn i = fn i []

{- | Convert Boolean Literals

> (True, False)

becomes

> (False, True)
-}
selectBLitOps :: Module_ -> [MuOp]
selectBLitOps = selectValOps isLit convert
  where
    isLit :: LHsExpr GhcPs -> Bool
    isLit (GHC.L _ (HsVar _ (GHC.L _ (GHC.Unqual n)))) = GHC.occNameString n == "True" || GHC.occNameString n == "False"
    isLit _ = False
    convert (WrpExpr (GHC.L l1 (HsVar x (GHC.L l2 (GHC.Unqual n)))))
        | GHC.occNameString n == "True" =
            [WrpExpr (GHC.L l1 (HsVar x (GHC.L l2 (GHC.Unqual (GHC.mkVarOcc "False")))))]
        | GHC.occNameString n == "False" =
            [WrpExpr (GHC.L l1 (HsVar x (GHC.L l2 (GHC.Unqual (GHC.mkVarOcc "True")))))]
        | otherwise = error "Filter of Booleans doesn't work"
    convert _ = error "Filter of Booleans doesn't work"

{- | Negating boolean in if/else statements

> if True then 1 else 0

becomes

> if True then 0 else 1
-}
selectIfElseBoolNegOps :: Module_ -> [MuOp]
selectIfElseBoolNegOps = selectValOps isIf convert
  where
    isIf :: LHsExpr GhcPs -> Bool
    --
    isIf (GHC.L _ (HsIf{})) = True
    isIf _ = False
    convert (WrpExpr (GHC.L l (HsIf s e1 e2 e3))) = [WrpExpr (GHC.L l (HsIf s e1 e3 e2))]
    convert _ = []

{- | Generate all operators for permuting and removal of pattern guards from
function definitions

> myFn (x:xs) = False
> myFn _ = True

becomes

> myFn _ = True
> myFn (x:xs) = False

> myFn _ = True

> myFn (x:xs) = False
-}
selectFnMatches :: Module_ -> [MuOp]
selectFnMatches = selectValOps isFunct convert
  where
    isFunct :: LHsDecl GhcPs -> Bool
    isFunct (GHC.L _ (ValD _ (FunBind{}))) = True
    isFunct _ = False
    convert (WrpDecl (GHC.L l (ValD x (FunBind ext id mg)))) = undefined -- map (ValD x (FunBind ext id mg)) $ filter (/= ms) (permutations ms ++ removeOneElem ms)
    convert _ = []

{- | Generate all operators for permuting symbols like binary operators
Since we are looking for symbols, we are reasonably sure that it is not
locally bound to a variable.
-}
selectSymbolFnOps :: Module_ -> [String] -> [MuOp]
selectSymbolFnOps m s = undefined -- selectValOps isBin convert m
--   where isBin :: Name_ -> Bool
--         isBin (Symbol _l n) | n `elem` s = True
--         isBin _ = False
--         convert (Symbol l n) = map (Symbol l) $ filter (/= n) s
--         convert _ = []

{- | Generate all operators for permuting commonly used functions (with
identifiers).
-}
selectIdentFnOps :: Module_ -> [String] -> [MuOp]
selectIdentFnOps m s = undefined -- selectValOps isCommonFn convert m
  where
    isCommonFn :: LHsExpr GhcPs -> Bool
    isCommonFn (GHC.L _ (HsVar _ (GHC.L _ (GHC.Unqual n)))) | GHC.occNameString n `elem` s = True
    isCommonFn _ = False

--       convert (HsVar _ (GHC.L l (GHC.Unqual n))) = undefined --map  (HsVar lv_ . UnQual lu_ . Ident li_) $ filter (/= GHC.occNameString n) s
--       convert _ = []

{- | Generate all operators depending on whether it is a symbol or not.
selectFunctionOps :: [FnOp] -> Module_ -> [MuOp]
selectFunctionOps fo f = concatMap (selectIdentFnOps f) idents ++ concatMap (selectSymbolFnOps f) syms
  where idents = map _fns $ filter (\a -> _type a == FnIdent) fo
        syms = map _fns $ filter (\a -> _type a == FnSymbol) fo
-}

-- (Var l (UnQual l (Ident l "ab")))
-- (App l (Var l (UnQual l (Ident l "head"))) (Var l (UnQual l (Ident l "b"))))
-- (App l (App l (Var l (UnQual l (Ident l "head"))) (Var l (UnQual l (Ident l "a")))) (Var l (UnQual l (Ident l "b")))))
-- (InfixApp l (Var l (UnQual l (Ident l "a"))) (QVarOp l (UnQual l (Symbol l ">"))) (Var l (UnQual l (Ident l "b"))))
-- (InfixApp l (Var l (UnQual l (Ident l "a"))) (QVarOp l (UnQual l (Ident l "x"))) (Var l (UnQual l (Ident l "b"))))

{- | Generate sub-arrays with one less element except when we have only
a single element.
-}
removeOneElem :: (Eq t) => [t] -> [[t]]
removeOneElem [_] = []
removeOneElem l = choose l (length l - 1)

-------------------------------------------------------------------------------
-- Mutation on Literals
-------------------------------------------------------------------------------

reverseStringLiteral :: HsLit GhcPs -> HsLit GhcPs
reverseStringLiteral (HsString _ fs) = HsString NoSourceText (GHC.mkFastStringByteString (BS.reverse (GHC.bytesFS fs)))
reverseStringLiteral x = x

greverseStringLiteral :: forall a. (Typeable a) => a -> a
greverseStringLiteral = mkT reverseStringLiteral

-------------------------------------------------------------------------------
-- Mutation on Pattern Matches
-------------------------------------------------------------------------------

reverseClauses :: MatchGroup GhcPs (LHsExpr GhcPs) -> MatchGroup GhcPs (LHsExpr GhcPs)
reverseClauses (MG _ (GHC.L l body)) = MG GHC.Generated (GHC.L l (reverse body))

greverseClauses :: forall a. (Typeable a) => a -> a
greverseClauses = mkT reverseClauses

-------------------------------------------------------------------------------
-- Mutation on + and -
-------------------------------------------------------------------------------

swapPlusMinusOperator :: HsExpr GhcPs -> HsExpr GhcPs
swapPlusMinusOperator (HsVar _ (GHC.L l (GHC.Unqual v))) = HsVar NoExtField (GHC.L l (GHC.Unqual (handleOccName v)))
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

swapIfElse :: HsExpr GhcPs -> HsExpr GhcPs
swapIfElse (HsIf _ i t e) = HsIf noAnn i e t
swapIfElse x = x

gswapIfElse :: forall a. (Typeable a) => a -> a
gswapIfElse = mkT swapIfElse

-------------------------------------------------------------------------------
-- Combined
-------------------------------------------------------------------------------

-- | Apply the given mutation operator to the Haskell module
mutate' :: MuVariant -> Module_ -> Module_
mutate' ReverseString = everywhere greverseStringLiteral
mutate' ReverseClausesInPatternMatch = everywhere greverseClauses
mutate' SwapPlusMinus = everywhere gswapPlusMinusOperator
mutate' SwapIfElse = everywhere gswapIfElse
