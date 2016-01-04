
import Prelude hiding (lex)
import System.Environment
import Data.List
import Lex (lex)
import Parse (parse)
import Scope (scope)
import Emit (emit)


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
    , (ext "scope" file, show scoped)
    , (ext "h" file, emit "h" (name file) scoped)
    , (ext "c" file, emit "c" (name file) scoped)
    ]
  where
    lexed = lex input
    parsed = parse input
    scoped = scope parsed

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
