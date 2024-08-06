{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

{- |
Module         : Test.Mendel.Mutation
Description    : Apply mutation operators to Haskell modules

This module provides the functionality to traverse Haskell modules and apply mutation operators.
-}
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
import Data.Ratio
import Data.Typeable
import GHC.Core.Utils (getIdFromTrivialExpr_maybe)
import GHC.Data.FastString qualified as GHC
import GHC.Driver.Backpack.Syntax (HsUnitDecl (DeclD))
import GHC.Hs
import GHC.Types.Basic qualified as GHC
import GHC.Types.Name.Occurrence qualified as GHC
import GHC.Types.Name.Reader qualified as GHC
import GHC.Types.SourceText
import GHC.Types.SrcLoc qualified as GHC
import Language.Haskell.GhclibParserEx.GHC.Hs.ExtendInstances
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
choose xs n = filter (\x -> astEq (length x) n) $ subsequences xs

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
mutate (v, op) (_v, _s, m) = undefined -- map (v,getSpan op,) $ once (mkMpMuOp op) m \\ [m]

-- | Returns all mutation operators
applicableOps ::
    -- | Configuration
    Config ->
    -- | Module to mutate
    Module_ ->
    -- | Returns mutation operators
    [(MuVariant, MuOp)]
applicableOps config ast = undefined -- relevantOps ast opsList
  where
    opsList =
        concatMap
            spread
            [ (MutatePatternMatch, selectFnMatches ast)
            , (MutateValues, selectLiteralOps ast)
            , -- (MutateFunctions, selectFunctionOps (muOp config) ast),
              (MutateNegateIfElse, selectIfElseBoolNegOps ast)
            ]

-- (MutateNegateGuards, selectGuardedBoolNegOps ast)

-- | Look for literal values in AST, and return applicable MuOp transforms.
selectLiteralOps :: Module_ -> [MuOp]
selectLiteralOps m = selectLitOps m ++ selectBLitOps m

{- | Look for literal values in AST, and return applicable MuOp transforms.
Unfortunately booleans are not handled here.
-}
selectLitOps :: Module_ -> [MuOp]
selectLitOps m = concat [x ==>* convert x | x <- WrpExpr <$> listify isLit m]
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
    convert (WrpExpr (GHC.L l (HsLit p (HsInt x i)))) =
        map (changeIntegral (WrpExpr (GHC.L l (HsLit p (HsInt x i))))) $
            nub [calcIntLit (+) i 1, calcIntLit (-) i 1, mkIntegralLit 0, mkIntegralLit 1]
    convert (WrpExpr (GHC.L l (HsLit p (HsIntPrim x i)))) = map (changeInt (WrpExpr (GHC.L l (HsLit p (HsIntPrim x i))))) $ nub [i + 1, i - 1, 0, 1]
    convert (WrpExpr (GHC.L l (HsLit p (HsChar x c)))) = map (changeChar (WrpExpr (GHC.L l (HsLit p (HsChar x c))))) [pred c, succ c]
    convert (WrpExpr (GHC.L l (HsLit p (HsCharPrim x c)))) = map (changeChar (WrpExpr (GHC.L l (HsLit p (HsCharPrim x c))))) [pred c, succ c]
    convert (WrpExpr (GHC.L l (HsLit p (HsFloatPrim x f)))) =
        map (changeFracLit (WrpExpr (GHC.L l (HsLit p (HsFloatPrim x f))))) $
            nub [calcFracLit (+) f 1, calcFracLit (-) f 1, mkTHFractionalLit 0, mkTHFractionalLit 1]
    convert (WrpExpr (GHC.L l (HsLit p (HsDoublePrim x f)))) =
        map (changeFracLit (WrpExpr (GHC.L l (HsLit p (HsDoublePrim x f))))) $
            nub [calcFracLit (+) f 1, calcFracLit (-) f 1, mkTHFractionalLit 0, mkTHFractionalLit 1]
    convert (WrpExpr (GHC.L l (HsLit p (HsString x _)))) = [WrpExpr (GHC.L l (HsLit p (HsString x (GHC.fsLit ""))))] -- was map over the List [""] before
    convert (WrpExpr (GHC.L l (HsLit p (HsStringPrim x _)))) = [WrpExpr (GHC.L l (HsLit p (HsStringPrim x (GHC.bytesFS $ GHC.fsLit ""))))] -- was map over the List [""] before
    convert (WrpExpr (GHC.L l (HsLit p (HsWordPrim x i)))) = map (changeInt (WrpExpr (GHC.L l (HsLit p (HsWordPrim x i))))) $ nub [i + 1, i - 1, 0, 1]
    convert (WrpExpr (GHC.L l (HsLit p (HsInteger x i t)))) = map (changeInt (WrpExpr (GHC.L l (HsLit p (HsInteger x i t))))) $ nub [i + 1, i - 1, 0, 1]
    convert (WrpExpr (GHC.L l (HsLit p (HsRat x f t)))) =
        map (changeFracLit (WrpExpr (GHC.L l (HsLit p (HsRat x f t))))) $
            nub [calcFracLit (+) f 1, calcFracLit (-) f 1, mkTHFractionalLit 0, mkTHFractionalLit 1]
    convert (WrpExpr (GHC.L l (HsLit p (HsInt64Prim x i)))) = map (changeInt (WrpExpr (GHC.L l (HsLit p (HsInt64Prim x i))))) $ nub [i + 1, i - 1, 0, 1]
    convert (WrpExpr (GHC.L l (HsLit p (HsWord64Prim x i)))) = map (changeInt (WrpExpr (GHC.L l (HsLit p (HsWord64Prim x i))))) $ nub [i + 1, i - 1, 0, 1]
    convert _ = error "No HsLit"
    calcIntLit :: (Integer -> Integer -> Integer) -> IntegralLit -> Integer -> IntegralLit
    calcIntLit f (IL _ _ 0) i = mkIntegralLit $ f 0 i
    calcIntLit f (IL _ neg val) i
        | neg = mkIntegralLit $ f (-val) i
        | otherwise = mkIntegralLit $ f val i
    calcFracLit ::
        (Ratio Integer -> Ratio Integer -> Ratio Integer) -> FractionalLit -> Ratio Integer -> FractionalLit
    calcFracLit f x i = mkTHFractionalLit $ f (rationalFromFractionalLit x) i
    changeIntegral :: Exp_ -> IntegralLit -> Exp_
    changeIntegral (WrpExpr (GHC.L l (HsLit p (HsInt x _)))) i = WrpExpr (GHC.L l (HsLit p (HsInt x i)))
    changeIntegral _ _ = error "false type"
    changeInt :: Exp_ -> Integer -> Exp_
    changeInt (WrpExpr (GHC.L l (HsLit p (HsIntPrim x _)))) i = WrpExpr (GHC.L l (HsLit p (HsIntPrim x i)))
    changeInt (WrpExpr (GHC.L l (HsLit p (HsWordPrim x _)))) i = WrpExpr (GHC.L l (HsLit p (HsWordPrim x i)))
    changeInt (WrpExpr (GHC.L l (HsLit p (HsInteger x _ t)))) i = WrpExpr (GHC.L l (HsLit p (HsInteger x i t)))
    changeInt (WrpExpr (GHC.L l (HsLit p (HsInt64Prim x _)))) i = WrpExpr (GHC.L l (HsLit p (HsInt64Prim x i)))
    changeInt (WrpExpr (GHC.L l (HsLit p (HsWord64Prim x _)))) i = WrpExpr (GHC.L l (HsLit p (HsWord64Prim x i)))
    changeInt _ _ = error "false type"
    changeChar :: Exp_ -> Char -> Exp_
    changeChar (WrpExpr (GHC.L l (HsLit p (HsChar x _)))) c = WrpExpr (GHC.L l (HsLit p (HsChar x c)))
    changeChar (WrpExpr (GHC.L l (HsLit p (HsCharPrim x _)))) c = WrpExpr (GHC.L l (HsLit p (HsCharPrim x c)))
    changeChar _ _ = error "false type"
    changeFracLit :: Exp_ -> FractionalLit -> Exp_
    changeFracLit (WrpExpr (GHC.L l (HsLit p (HsFloatPrim x _)))) i = WrpExpr (GHC.L l (HsLit p (HsFloatPrim x i)))
    changeFracLit (WrpExpr (GHC.L l (HsLit p (HsDoublePrim x _)))) i = WrpExpr (GHC.L l (HsLit p (HsDoublePrim x i)))
    changeFracLit (WrpExpr (GHC.L l (HsLit p (HsRat x _ t)))) i = WrpExpr (GHC.L l (HsLit p (HsRat x i t)))
    changeFracLit _ _ = error "false type"

{- | Convert Boolean Literals

> (True, False)

becomes

> (False, True)
-}
selectBLitOps :: Module_ -> [MuOp]
selectBLitOps m = concat [x ==>* convert x | x <- WrpExpr <$> listify isLit m]
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
selectIfElseBoolNegOps m = concat [x ==>* convert x | x <- WrpExpr <$> listify isIf m]
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
selectFnMatches m = concat [x ==>* convert x | x <- WrpDecl <$> listify isFunct m]
  where
    isFunct :: LHsDecl GhcPs -> Bool
    isFunct (GHC.L _ (ValD _ (FunBind{}))) = True
    isFunct _ = False
    convert (WrpDecl (GHC.L l1 (ValD x (FunBind ext i (MG exts (GHC.L l2 alts)))))) =
        map (changeMG (WrpDecl (GHC.L l1 (ValD x (FunBind ext i (MG exts (GHC.L l2 alts))))))) $
            filter (not . astEq alts) (permutations alts ++ removeOneElem alts)
    convert _ = []

    changeMG :: Decl_ -> [LMatch GhcPs (LHsExpr GhcPs)] -> Decl_
    changeMG (WrpDecl (GHC.L l1 (ValD x (FunBind ext i (MG exts (GHC.L l2 _)))))) xs = WrpDecl (GHC.L l1 (ValD x (FunBind ext i (MG exts (GHC.L l2 xs)))))
    changeMG _ _ = error "Wrong declaration"

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
selectIdentFnOps m s = concat [x ==>* convert x | x <- WrpExpr <$> listify isCommonFn m]
  where
    isCommonFn :: LHsExpr GhcPs -> Bool
    isCommonFn (GHC.L _ (HsVar _ (GHC.L _ (GHC.Unqual n)))) | GHC.occNameString n `elem` s = True
    isCommonFn _ = False
    convert = undefined -- (HsVar _ (GHC.L l (GHC.Unqual n))) = map  (HsVar lv_ . UnQual lu_ . Ident li_) $ filter (/= GHC.occNameString n) s
    --    convert _ = []

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
removeOneElem :: (Data t) => [t] -> [[t]]
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
