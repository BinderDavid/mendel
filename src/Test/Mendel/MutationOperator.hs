{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}

-- | Mutation operators
module Test.Mendel.MutationOperator (
    MuOp,
    Mutable (..),
    (==>*),
    (*==>*),
    (~~>),
    mkMpMuOp,
    same,
    Module_,
    Exp_ (..),
    Decl_ (..),
    getSpan,
) where

import Control.Monad (MonadPlus, mzero)
import Data.Generics qualified as G
import GHC.Hs
import GHC.Types.SrcLoc
import GHC.Utils.Outputable
import Language.Haskell.GhclibParserEx.GHC.Hs.ExtendInstances

type Module_ = HsModule GhcPs

-- wrapper for 'LHsExpr GhcPs'
newtype Exp_ = WrpExpr {getExpr :: LHsExpr GhcPs}

-- wrapper for 'LHsDecl GhcPs'
newtype Decl_ = WrpDecl {getDecl :: LHsDecl GhcPs}

-- | MuOp constructor used to specify mutation transformation
data MuOp
    = E Exp_ Exp_
    | D Decl_ Decl_

instance Eq MuOp where
    (==) (E (WrpExpr x1) (WrpExpr y1)) (E (WrpExpr x2) (WrpExpr y2)) = astEq x1 x2 && astEq y1 y2
    (==) (D (WrpDecl x1) (WrpDecl y1)) (D (WrpDecl x2) (WrpDecl y2)) = astEq x1 x2 && astEq y1 y2
    (==) _ _ = False

-- | Show a specified mutation
showM :: (Outputable a, Outputable b) => a -> b -> String
showM s t = "{\n" ++ showPprUnsafe s ++ "\n} ==> {\n" ++ showPprUnsafe t ++ "\n}"

-- | MuOp instance for Show
instance Show MuOp where
    show (E (WrpExpr x) (WrpExpr y)) = showM x y
    show (D (WrpDecl x) (WrpDecl y)) = showM x y

-- | getSpan retrieve the span as a tuple
getSpan :: MuOp -> (Int, Int, Int, Int)
getSpan m = (startLine, startCol, endLine, endCol)
  where
    startLine = getInt srcSpanStartLine lspan
    startCol = getInt srcSpanStartCol lspan
    endLine = getInt srcSpanEndLine lspan
    endCol = getInt srcSpanEndCol lspan
    getInt :: (RealSrcSpan -> Int) -> Maybe RealSrcSpan -> Int
    getInt f (Just x) = f x
    getInt _ Nothing = error "No Source Span"
    getSpan' (E (WrpExpr a) _) = getLocA a
    getSpan' (D (WrpDecl a) _) = getLocA a
    lspan = srcSpanToRealSrcSpan $ getSpan' m

{- | The function `same` applies on a `MuOP` determining if transformation is
between same values.
-}
same :: MuOp -> Bool
same (E (WrpExpr x) (WrpExpr y)) = astEq x y
same (D (WrpDecl x) (WrpDecl y)) = astEq x y

-- | A wrapper over mkMp
mkMpMuOp :: (MonadPlus m, G.Typeable a) => MuOp -> a -> m a
mkMpMuOp (E x y) = G.mkMp (getExpr x ~~> getExpr y)
mkMpMuOp (D x y) = G.mkMp (getDecl x ~~> getDecl y)

-- | Mutation operation representing translation from one fn to another fn.
class Mutable a where
    (==>) :: a -> a -> MuOp

{- | The function `==>*` pairs up the given element with all elements of the
second list, and applies `==>` on them.
-}
(==>*) :: (Mutable a) => a -> [a] -> [MuOp]
(==>*) x = map (x ==>)

{- | The function `*==>*` pairs up all elements of first list with all elements
of second list and applies `==>` between them.
-}
(*==>*) :: (Mutable a) => [a] -> [a] -> [MuOp]
xs *==>* ys = concatMap (==>* ys) xs

{- | The function `~~>` accepts two values, and returns a function
that if given a value equal to first, returns second
we handle x ~~> x separately
-}
(~~>) :: (MonadPlus m, G.Data a) => a -> a -> a -> m a
x ~~> y = \z -> if astEq z x then return y else mzero

-- | Exp instance for Mutable
instance Mutable Exp_ where
    e1 ==> e2 = E e1 e2

-- | Decl instance for Mutable
instance Mutable Decl_ where
    d1 ==> d2 = D d1 d2
