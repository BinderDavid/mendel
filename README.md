# Mendel
[![Haskell-CI](https://github.com/BinderDavid/mendel/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/BinderDavid/mendel/actions/workflows/haskell-ci.yml)

Mendel is a library for introducing faults into Haskell sourcecode, which is sometimes called mutant generation.
Its implementation is based on two central libraries:

- `ghc-lib-parser` for parsing Haskell source code and for the AST representation of Haskell modules and expressions.
- `syb` The scrap-your-boilerplate framework for writing generic transformations on ASTs without the boilerplate.

The ability to inject faults has many use cases, for example:

- Mutation testing provides an alternative quality metric for test suites, apart from the more familiar code coverage metric.
- The development of fault localization techniques requires a dataset of programs with known faults.
  These can be either collected from real-world examples, or synthesized by artificially introducing (seeding) faults into correct programs.
- The development of automatic program repair systems also requires datasets of programs with known faults.


## Inspiration

This library is inspired by previous work:

- MuCheck: An extensible tool for mutation testing of Haskell programs (2014) by Duc Le, Mohammad Amin Alipour, Rahul Gopinath and Alex Groce [ACM DL](https://dl.acm.org/doi/10.1145/2610384.2628052)
- MuCheck, the tool based on the paper [Hackage](https://hackage.haskell.org/package/MuCheck)

Their implementation, however, was based on the `haskell-src-exts` library which is no longer maintained.
This means that code which uses modern GHC-specific features can no longer be analyzed using this library.
It was therefore necessary to reimplement the mutation logic using the data-structures that GHC uses, which are provided in the `ghc-lib-parser` library.

## Limitations

This library works on the parsed AST, not the renamed or typechecked ASTs.
For this reason we can only specify mutations which only require the parsed AST.

## Example

A small binary is provided which can be used to test the effect of various mutation operators on real code.

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

