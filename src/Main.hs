
import Prelude hiding (lex, read, const)
import System.Environment
import System.Exit
import System.FilePath
import Control.Exception
import Control.Monad
import Data.Bifunctor
import qualified Lex   as L
import qualified Parse as P
import qualified Scope as S
import qualified Emit  as E
import Stage
import Result
import Pos


-- Compilation stages
--lex   :: Stage (FilePath, [String])  (FilePath, [L.Token])
--parse :: Stage (FilePath, [L.Token]) (FilePath, P.Tree)
--scope :: Stage (FilePath, P.Tree)    (FilePath, S.Module)
--emit  :: Stage (FilePath, S.Module)  [(FilePath, [String])]
--
--lex   = stage $ fst &&& uncurry L.lex
--parse = stage $ fst &&& uncurry P.parse
--scope = stage $ second S.scope
--emit  = stage $ uncurry E.emit

--read :: FilePath -> Result (Positional String) [String]
--read fp = 

debug :: Show a => String -> FilePath -> a -> IO ()
debug x fp a = writeFile (fp -<.> x) (show a)

read :: FilePath -> Result e (IO [String])
read fp = ok $ lines <$> readFile fp

write :: FilePath -> [String] -> Result e (IO ())
write fp ss = ok $ writeFile fp (unlines ss)

compile :: FilePath -> IO (Result (Positional String) ())
compile fp = do
    source <- lines <$> readFile fp
    let lex   = L.lex   fp          source
    sequence_ $ debug "lex" fp <$> lex
    let parse = P.parse fp      =<< lex
    sequence_ $ debug "parse" fp <$> parse
    let scope = ok . S.scope    =<< parse
    sequence_ $ debug "scope" fp <$> scope
    let emit  = ok . E.emit fp  =<< scope
    sequence_ $ debug "emit" fp <$> emit
    
    mapM (mapM_ (uncurry writeFile . second unlines)) emit
    --mapM_ (uncurry writeFile . second (map (second unlines))) <$> emit
    

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
        exitFailure

    result <- compile $ head args
    check $ first snd result
--
--    try (compile $ head args) >>= \case
--        Right a                    -> return a
--        Left (ex :: SomeException) -> do
--            putStr $ "\n" ++ show ex ++ "\n"
--            exit BadCompile
--
