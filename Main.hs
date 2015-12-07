
import System.Environment
import Data.List
import Prelude hiding (lex)
import Lex
import Parse

ext :: String -> FilePath -> FilePath
ext new path = rawpath ++ "." ++ new
  where
    old = ".sp"
    rawpath
      | isSuffixOf old path = take (length path - length old) path
      | otherwise = path

compile :: (FilePath, String) -> [(FilePath, String)]
compile (file, input) = [
    (ext "c" file, input),
    (ext "h" file, input),
    (ext "lex" file, show $ lex input),
    (ext "parse" file, show $ parse $ lex input)]

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
