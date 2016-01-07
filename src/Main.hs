
import Prelude hiding (lex, read, const)
import System.Environment
import System.Exit
import Control.Exception
import Control.Arrow
import Control.Monad
import qualified Lex   as L
import qualified Parse as P
import qualified Scope as S
import qualified Emit  as E
import Stage


-- Compilation stages
lex   :: Stage (FilePath, [String])  (FilePath, [L.Token])
parse :: Stage (FilePath, [L.Token]) (FilePath, P.Tree)
scope :: Stage (FilePath, P.Tree)    (FilePath, S.Module)
emit  :: Stage (FilePath, S.Module)  [(FilePath, [String])]

lex   = stage $ fst &&& uncurry L.lex
parse = stage $ fst &&& uncurry P.parse
scope = stage $ second S.scope
emit  = stage $ uncurry E.emit

compile :: Stage (FilePath, [String]) [(FilePath, [String])]
compile
  =   lex   >>> dump "lex"
  >>> parse >>> dump "parse"
  >>> scope >>> dump "scope"
  >>> emit

compileFile :: FilePath -> IO ()
compileFile fp = pipe fp
  $   read
  >>> compile
  >>> mapA_ write


-- Program entry point
data Status
  = OK
  | BadArgs
  | BadCompile
  deriving (Show, Enum)

exit :: Status -> IO a
exit = exitWith . ExitFailure . fromEnum

help :: String -> String
help prog = "\
    \usage: " ++ prog ++ " [options] file\n\
    \file: file to compile\n"

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1) $ do
        prog <- help <$> getProgName
        putStr prog
        exit BadArgs

    try (compileFile $ head args) >>= \case
        Right a                    -> return a
        Left (ex :: SomeException) -> do
            putStr $ "\n" ++ show ex ++ "\n"
            exit BadCompile

