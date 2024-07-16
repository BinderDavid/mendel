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
          , Name_
          , Exp_
          , Decl_
          , Literal_
         -- , GuardedRhs_
         , getSpan
          ) where

import qualified Data.Generics as G
import Control.Monad (MonadPlus, mzero)

import GHC.Hs --(HsModule, HsExpr, HsDecl, HsLit, GhcPs)
import GHC.Types.Name.Occurrence (OccName, occNameString)
import Language.Haskell.Syntax.Expr (GRHS)
import GHC.Parser.Annotation
import Language.Haskell.TH.Ppr
import Data.Data (Data(toConstr))
import GHC.Utils.Outputable


type Module_ = HsModule GhcPs
type Name_ = OccName
type Exp_ = HsExpr GhcPs
type Decl_ = HsDecl GhcPs
type Literal_ = HsLit GhcPs
-- type GuardedRhs_ = GRHS GhcPs (LocatedA GhcPs) 

instance Eq Module_ where
  (==) x y = toConstr x == toConstr y

instance Eq Exp_ where
  (==) x y = toConstr x == toConstr y

instance Eq Decl_ where
  (==) x y = toConstr x == toConstr y


-- | MuOp constructor used to specify mutation transformation
data MuOp = N  (Name_, Name_)
          | E  (Exp_, Exp_)
          | D  (Decl_, Decl_)
          | L  (Literal_, Literal_)
         -- | G  (GuardedRhs_, GuardedRhs_)
  deriving Eq


-- How do I get the Annotated (a SrcSpanInfo) on apply's signature?
-- | getSpan retrieve the span as a tuple
getSpan :: MuOp -> (Int, Int, Int, Int)
getSpan m = undefined --(startLine, startCol, endLine, endCol)
--   where (endLine, endCol) = srcSpanEnd lspan
--         (startLine, startCol) = srcSpanStart lspan
--         getSpan' (N  (a,_)) = ann a
--         getSpan' (E  (a,_)) = ann a
--         getSpan' (D  (a,_)) = ann a
--         getSpan' (L  (a,_)) = ann a
--         -- getSpan' (G  (a,_)) = ann a
--         lspan = srcInfoSpan $ getSpan' m

-- | The function `same` applies on a `MuOP` determining if transformation is
-- between same values.
same :: MuOp -> Bool
same (N (x,y)) = x == y
same (E (x,y)) = x == y
same (D (x,y)) = x == y
same (L (x,y)) = x == y
-- same (G (x,y)) = x == y

-- | A wrapper over mkMp
mkMpMuOp :: (MonadPlus m, G.Typeable a) => MuOp -> a -> m a
mkMpMuOp (N (x,y)) = G.mkMp (x ~~> y) 
mkMpMuOp (E (x,y)) = G.mkMp (x ~~> y) 
mkMpMuOp (D (x,y)) = G.mkMp (x ~~> y) 
mkMpMuOp (L (x,y)) = G.mkMp (x ~~> y) 
-- mkMpMuOp (G (x,y)) = G.mkMp (x ~~> y) 

-- | Show a specified mutation
showM :: (Outputable  a, Outputable a1) => (a, a1) -> String
showM (s, t) = "{\n" ++ showPprUnsafe s ++ "\n} ==> {\n" ++ showPprUnsafe t ++ "\n}"

-- | MuOp instance for Show
instance Show MuOp where
  show (N m) = showM m
  show (E m) = showM m
  show (D m) = showM m
  show (L m) = showM m
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
(~~>) :: (MonadPlus m, Eq a) => a -> a -> a -> m a
x ~~> y = \z -> if z == x then return y else mzero

-- | Name instance for Mutable
instance Mutable Name_ where
  (==>) = (N .) . (,)

-- | Exp instance for Mutable
instance Mutable Exp_ where
  (==>) = (E .) . (,)

-- | Exp instance for Mutable
instance Mutable Decl_ where
  (==>) = (D .) . (,)

-- | Literal instance for Mutable
instance Mutable Literal_ where
  (==>) = (L .) . (,)

-- | GuardedRhs instance for Mutable
-- instance Mutable GuardedRhs_ where
--   (==>) = (G .) . (,)