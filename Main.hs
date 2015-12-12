
import System.Environment
import Data.List
import Prelude hiding (lex)
import Lex
import Parse
import Gen

ext :: String -> FilePath -> FilePath
ext new path = rawpath ++ "." ++ new
  where
    old = ".sp"
    rawpath
      | isSuffixOf old path = take (length path - length old) path
      | otherwise           = path

compile :: (FilePath, String) -> [(FilePath, String)]
compile (file, input) =
    [ (ext "lex" file, show lexed)
    , (ext "parse" file, show parsed)
    , (ext "h" file, gen "h" parsed)
    , (ext "c" file, gen "c" parsed)
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
