
import Prelude hiding (lex, read, const)
import System.Environment
import System.Exit
import System.FilePath
import Control.Monad
import Data.Bifunctor
import qualified Lex   as L
import qualified Parse as P
import qualified Scope as S
import qualified Emit  as E
import Result
import Pos
import Nested


-- Compilation stages
read :: FilePath -> IO [String]
read = fmap lines . readFile

write :: (FilePath, [String]) -> IO ()
write = uncurry writeFile . second unlines

dump :: Show a => String -> FilePath -> a -> IO ()
dump x fp a = write (fp -<.> x, [show a])

compile :: FilePath -> IO (Result (Positional String) ())
compile fp = unnest $ do
    source  <- lift1 $ read fp
    lexed   <- lift2 $ L.lex fp source
    parsed  <- lift2 $ P.parse fp lexed
    scoped  <- pure  $ S.scope parsed
    emitted <- pure  $ E.emit fp scoped
    lift1 $ dump "lex"   fp lexed
    lift1 $ dump "parse" fp parsed
    lift1 $ dump "scope" fp scoped
    mapM_ (lift1 . write) emitted
    

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

