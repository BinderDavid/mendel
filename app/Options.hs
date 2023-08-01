module Options where

import Options.Applicative

data Options =
    MutateFile FilePath
    deriving (Eq, Show)

optionsParser :: Parser Options
optionsParser = MutateFile <$> argument str (metavar "FILE")

optionsParser' :: ParserInfo Options
optionsParser' = info optionsParser fullDesc
