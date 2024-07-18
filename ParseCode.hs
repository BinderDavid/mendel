module ParseCode where

import Test.Mendel.Parser (parseModule)
import GHC.Utils.Outputable (showSDoc, ppr, defaultSDocContext)
import System.IO (hPutStrLn, stderr)
import GHC.Hs (HsModule)

main :: IO ()
main = do
    let code = "myBool :: Bool\nmyBool = True"
    result <- parseCode code
    case result of
        Just ast -> putStrLn $ showAst ast
        Nothing -> hPutStrLn stderr "Parse error"

parseCode :: String -> IO (Maybe (Located (HsModule GhcPs)))
parseCode code = do
    let filePath = "Temp.hs"
    writeFile filePath code
    parseModule filePath

showAst :: Located (HsModule GhcPs) -> String
showAst = showSDoc defaultSDocContext . ppr