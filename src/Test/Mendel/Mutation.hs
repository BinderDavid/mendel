module Test.Mendel.Mutation where

import GHC.Hs qualified as GHC
import Language.Haskell.Syntax.Lit qualified as Hs
import GHC.Types.SourceText

import Data.Generics.Schemes (everywhere)
import Data.Generics.Aliases (mkT)
import Data.Typeable
import Data.String

reverseStringLiteral :: Hs.HsLit GHC.GhcPs -> Hs.HsLit GHC.GhcPs
reverseStringLiteral (Hs.HsString _ fs) =  Hs.HsString NoSourceText (fromString  (reverse (show fs)))
reverseStringLiteral x = x

greverseStringLiteral :: forall a. Typeable a => a -> a
greverseStringLiteral = mkT reverseStringLiteral

mutate :: GHC.HsModule GHC.GhcPs -> GHC.HsModule GHC.GhcPs
mutate = everywhere greverseStringLiteral