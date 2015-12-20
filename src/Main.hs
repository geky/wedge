
import System.Environment
import Data.List
import Prelude hiding (lex)
import Rule
import Lex
import Parse
import Emit


ext :: String -> FilePath -> FilePath
ext new path = rawpath ++ "." ++ new
  where
    old = ".w"
    rawpath
      | isSuffixOf old path = take (length path - length old) path
      | otherwise           = path

compile :: (FilePath, String) -> [(FilePath, String)]
compile (file, input) =
    [ (ext "lex" file, unlines $ map show $ xlines lexed)
    , (ext "parse" file, unlines $ map show $ xlines parsed)
    , (ext "h" file, emit "h" parsed)
    , (ext "c" file, emit "c" parsed)
    ]
  where
    lexed = lex input
    parsed = parse lexed

help :: String -> String
help prog = "\
    \usage: " ++ prog ++ " [options] file\n\
    \file: file to compile\n"

main :: IO ()
main = do
    args <- getArgs

    if length args /= 1 then do
        prog <- getProgName
        putStr (help prog)
    else do
        input <- readFile (head args)
        mapM_ (uncurry writeFile) $ compile (head args, input)
