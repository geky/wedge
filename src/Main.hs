
import Prelude hiding (lex, read, const)
import System.Environment
import System.Exit
import System.FilePath
import Control.Monad
import Control.Arrow hiding (first, second)
import Data.Bifunctor
import qualified Lex   as L
import qualified Parse as P
import qualified Scope as S
import qualified Emit  as E
import Stage
import Result
import Pos


-- Compilation stages
compile :: FilePath -> IO (Result (Positional String) ())
compile fp = pipe fp () $
      read
  >>> stage L.lex   >>> dump "lex"
  >>> stage P.parse >>> dump "parse"
  >>> arr   S.scope >>> dump "scope"
  >>> stage (\fp -> ok . E.emit fp) >>> dump "emit"
  >>> mapA_ write
  
    

-- Program entry point
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
        exitFailure

    result <- compile $ head args
    check $ first snd result

