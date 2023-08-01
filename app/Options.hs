module Options ( Options(..), optionsParser) where

import Options.Applicative
import Test.Mendel.MutationOperator

data Options
  = MutateFile MuOp FilePath
  | Version
  deriving (Eq, Show)

mutateFileParser :: Parser Options
mutateFileParser = MutateFile ReverseString <$> argument str (metavar "FILE")

versionParser :: Parser Options
versionParser = Version <$ switch (long "version" <> help "Display version")

optionsParser :: ParserInfo Options
optionsParser = info (versionParser <|> mutateFileParser) fullDesc
