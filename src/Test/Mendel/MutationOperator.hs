{-#  LANGUAGE Rank2Types, FlexibleInstances, ConstraintKinds #-}
-- | Mutation operators
module Test.Mendel.MutationOperator (MuOp
          , Mutable(..)
          , (==>*)
          , (*==>*)
          , (~~>)
          , mkMpMuOp
          , same
          , Module_
          , Exp_
          , Decl_
         -- , GuardedRhs_
         , getSpan
          ) where

import qualified Data.Generics as G
import Control.Monad (MonadPlus, mzero)

import GHC.Hs
--import Language.Haskell.Syntax.Expr (GRHS)
--import Data.Data (Data(toConstr))
import GHC.Utils.Outputable
import Language.Haskell.GhclibParserEx.GHC.Hs.ExtendInstances
import GHC.Types.SrcLoc

type Module_ = HsModule GhcPs
type Exp_ = LHsExpr GhcPs
type Decl_ = LHsDecl GhcPs
-- type GuardedRhs_ = GRHS GhcPs (LocatedA GhcPs) 

-- | MuOp constructor used to specify mutation transformation
data MuOp = E  (Exp_, Exp_)
          | D  (Decl_, Decl_)
         -- | G  (GuardedRhs_, GuardedRhs_)

instance Eq MuOp where
  (==) (E (x1, y1)) (E (x2, y2)) = astEq x1 x2 && astEq y1 y2
  (==) (D (x1, y1)) (D (x2, y2)) = astEq x1 x2 && astEq y1 y2
  (==) (D _) (E _) = False
  (==) (E _) (D _) = False


-- How do I get the Annotated (a SrcSpanInfo) on apply's signature?
-- | getSpan retrieve the span as a tuple
getSpan :: MuOp -> (Int, Int, Int, Int)
getSpan m = (startLine, startCol, endLine, endCol) 
  where startLine = getInt srcSpanStartLine lspan
        startCol  = getInt srcSpanStartCol lspan
        endLine   = getInt srcSpanEndLine lspan
        endCol    = getInt srcSpanEndCol lspan
        getInt :: (RealSrcSpan -> Int) -> Maybe RealSrcSpan -> Int
        getInt f (Just x) = f x 
        getInt _ Nothing = error "No Source Span"
        getSpan' (E  (a,_)) = getLocA a
        getSpan' (D  (a,_)) = getLocA a
        -- getSpan' (G  (a,_)) = ann a
        lspan = srcSpanToRealSrcSpan $ getSpan' m

-- | The function `same` applies on a `MuOP` determining if transformation is
-- between same values.
same :: MuOp -> Bool
same (E (x,y)) = astEq x y
same (D (x,y)) = astEq x y
-- same (G (x,y)) = x == y

-- | A wrapper over mkMp
mkMpMuOp :: (MonadPlus m, G.Typeable a) => MuOp -> a -> m a
mkMpMuOp (E (x,y)) = G.mkMp (x ~~> y) 
mkMpMuOp (D (x,y)) = G.mkMp (x ~~> y)  
-- mkMpMuOp (G (x,y)) = G.mkMp (x ~~> y) 

-- | Show a specified mutation
showM :: (Outputable a, Outputable a1) => (a, a1) -> String
showM (s, t) = "{\n" ++ showPprUnsafe s ++ "\n} ==> {\n" ++ showPprUnsafe t ++ "\n}"

-- | MuOp instance for Show
instance Show MuOp where
  show (E m) = showM m
  show (D m) = showM m
  -- show (G p) = showM p

-- | Mutation operation representing translation from one fn to another fn.
class Mutable a where
  (==>) :: a -> a -> MuOp

-- | The function `==>*` pairs up the given element with all elements of the
-- second list, and applies `==>` on them.
(==>*) :: Mutable a => a -> [a] -> [MuOp]
(==>*) x = map (x ==>)

-- | The function `*==>*` pairs up all elements of first list with all elements
-- of second list and applies `==>` between them.
(*==>*) :: Mutable a => [a] -> [a] -> [MuOp]
xs *==>* ys = concatMap (==>* ys) xs

-- | The function `~~>` accepts two values, and returns a function
-- that if given a value equal to first, returns second
-- we handle x ~~> x separately
(~~>) :: (MonadPlus m, G.Data a) => a -> a -> a -> m a
x ~~> y = \z -> if astEq z x then return y else mzero

-- | Exp instance for Mutable
-- instance Mutable Exp_ where
--   (==>) = (E .) . (,)

-- -- | Exp instance for Mutable
-- instance Mutable Decl_ where
--   (==>) = (D .) . (,)

-- | GuardedRhs instance for Mutable
-- instance Mutable GuardedRhs_ where
--   (==>) = (G .) . (,)