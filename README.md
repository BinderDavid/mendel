# Mendel
[![Haskell-CI](https://github.com/BinderDavid/mendel/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/BinderDavid/mendel/actions/workflows/haskell-ci.yml)

Mendel is a library for introducing faults into Haskell Sourcecode, which is sometimes called mutant generation.
Its implementation is based on two central libraries:
- `ghc-lib-parser` for parsing Haskell source code and the AST representation of Haskell modules and expressions.
- `syb` The scrap-your-boilerplate framework for writing generic transformations on ASTs without the boilerplate

## Example

Notice how all the string literals are reversed in the mutated module

```console
> cabal run mendel -- src/Test/Mendel/Parser.hs
-------------------------------------------------------
BEFORE
-------------------------------------------------------
Parse succeeded
module Test.Mendel.Parser (
        parseModule
    ) where
import GHC.Parser qualified as GHC
import GHC.Parser.Lexer qualified as GHC
import GHC.Data.StringBuffer qualified as GHC
import GHC.Data.FastString qualified as GHC
import GHC.Types.SrcLoc qualified as GHC
import GHC.Utils.Outputable qualified as GHC
import GHC.Hs qualified as GHC
import GHC.Data.EnumSet ( empty )
import GHC.Utils.Error qualified as GHC
import System.Exit ( exitFailure )
parseModule ::
  FilePath -> IO (GHC.Located (GHC.HsModule GHC.GhcPs))
parseModule fp
  = do file <- readFile fp
       let parseResult = runParserModule file
       case parseResult of
         GHC.POk _state res -> pure res
         GHC.PFailed _state
           -> do putStrLn "Parse failed"
                 exitFailure
runParserModule ::
  String -> GHC.ParseResult (GHC.Located (GHC.HsModule GHC.GhcPs))
runParserModule str = runParser parserOpts str GHC.parseModule
parserOpts :: GHC.ParserOpts
parserOpts
  = GHC.mkParserOpts empty diagOpts [] False False False False
diagOpts :: GHC.DiagOpts
diagOpts
  = GHC.DiagOpts
      empty empty False False Nothing GHC.defaultSDocContext
runParser ::
  GHC.ParserOpts -> String -> GHC.P a -> GHC.ParseResult a
runParser opts str parser
  = GHC.unP parser parseState
  where
      filename = "<interactive>"
      location = GHC.mkRealSrcLoc (GHC.mkFastString filename) 1 1
      buffer = GHC.stringToStringBuffer str
      parseState = GHC.initParserState opts buffer location
-------------------------------------------------------
AFTER
-------------------------------------------------------
Parse succeeded
module Test.Mendel.Parser (
        parseModule
    ) where
import GHC.Parser qualified as GHC
import GHC.Parser.Lexer qualified as GHC
import GHC.Data.StringBuffer qualified as GHC
import GHC.Data.FastString qualified as GHC
import GHC.Types.SrcLoc qualified as GHC
import GHC.Utils.Outputable qualified as GHC
import GHC.Hs qualified as GHC
import GHC.Data.EnumSet ( empty )
import GHC.Utils.Error qualified as GHC
import System.Exit ( exitFailure )
parseModule ::
  FilePath -> IO (GHC.Located (GHC.HsModule GHC.GhcPs))
parseModule fp
  = do file <- readFile fp
       let parseResult = runParserModule file
       case parseResult of
         GHC.POk _state res -> pure res
         GHC.PFailed _state
           -> do putStrLn "\"deliaf esraP\""
                 exitFailure
runParserModule ::
  String -> GHC.ParseResult (GHC.Located (GHC.HsModule GHC.GhcPs))
runParserModule str = runParser parserOpts str GHC.parseModule
parserOpts :: GHC.ParserOpts
parserOpts
  = GHC.mkParserOpts empty diagOpts [] False False False False
diagOpts :: GHC.DiagOpts
diagOpts
  = GHC.DiagOpts
      empty empty False False Nothing GHC.defaultSDocContext
runParser ::
  GHC.ParserOpts -> String -> GHC.P a -> GHC.ParseResult a
runParser opts str parser
  = GHC.unP parser parseState
  where
      filename = "\">evitcaretni<\""
      location = GHC.mkRealSrcLoc (GHC.mkFastString filename) 1 1
      buffer = GHC.stringToStringBuffer str
      parseState = GHC.initParserState opts buffer location
```

