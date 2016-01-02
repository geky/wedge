
import System.Environment
import Data.List
import Prelude hiding (lex)
import Lex
import Parse
import Emit


ext :: String -> FilePath -> FilePath
ext new path = name path ++ "." ++ new

name :: String -> String
name path
  | isSuffixOf ".w" path = take (length path - length ".w") path
  | otherwise            = path

compile :: (FilePath, String) -> [(FilePath, String)]
compile (file, input) =
    [ (ext "lex" file, unlines $ map show lexed)
    , (ext "parse" file, unlines $ map show parsed)
    , (ext "h" file, emit "h" (name file) parsed)
    , (ext "c" file, emit "c" (name file) parsed)
    ]
  where
    lexed = lex input
    parsed = parse input

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
