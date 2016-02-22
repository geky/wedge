module Emit where

import Prelude hiding (lex)
import System.FilePath
import Data.Char
import Data.List
import Data.Maybe
import Type
import Expr
import Scope
  ( Stmt(..)
  , Block
  , Decl(..)
  , Module(..)
  )


-- Safe encoding to C symbols
escape :: String -> String
escape = (concat.) $ map $ \case
  '+' -> "add"
  '!' -> "not"
  c   -> pure c


-- Emitting definitions
args :: Struct -> String
args = (\s -> "("++s++")") . \case
    [] -> "void"
    ys -> intercalate ", " $ map var ys

members :: Struct -> String
members = (\s -> "{"++s++"}") . intercalate " " . map ((++";") . var)

array :: Type -> Maybe Int -> Maybe String -> String
array y n name = type' y ++ name' ++ "[" ++ n' ++ "]"
  where
    name' = fromMaybe "" ((" "++) <$> name)
    n'    = fromMaybe "" (show <$> n)

func :: Struct -> Struct -> Maybe String -> String
func x y name = type' (fromStruct y) ++ " (*" ++ name' ++ ")" ++ args x
  where name' = fromMaybe "" name

type' :: Type -> String
type' = \case
    Void         -> "void"
    Type "int"   -> "int"
    Type "uint"  -> "unsigned"
    Type "float" -> "double"
    Type t       -> t
    Struct ys    -> "struct " ++ members ys
    y            -> var (y, Nothing)

var :: (Type, Maybe Var) -> String
var (y, name) = case y of
    Array y n -> type' y ++ name' ++ "[" ++ n' ++ "]"
      where n' = fromMaybe "" (show <$> n)
    Func x y  -> type' (fromStruct y) ++ " (*" ++ name' ++ ")" ++ args x
      where name' = fromMaybe "" name
    y         -> type' y ++ name'
  where name' = fromMaybe "" ((" "++) <$> name)

expr :: Expr -> String
expr = \case
    Call e es   -> expr e ++ "(" ++ args ++ ")"
      where args = intercalate ", " $ map expr es
    Access e s  -> expr e ++ "." ++ s
    Var s       -> escape s
    Int i       -> show i
    Float f     -> show f
    Tuple es    -> "(char[]){" ++ entries ++ "}"
      where entries = intercalate ", " $ map expr es

stmt :: Stmt -> [String]
stmt = \case
    Expr e     -> [expr e ++ ";"]
    Assign l r -> [expr l ++ " = " ++ expr r ++ ";"]
    Return e   -> ["return " ++ expr e ++ ";"]
    Break      -> ["break;"]
    Continue   -> ["continue;"]
    If t l r -> concat
        [ ["if (" ++ expr t ++ ") {"]
        , block l
        , ["} else {"]
        , block r
        , ["}"]
        ]
    While t l -> concat
        [ ["while (" ++ expr t ++ ") {"]
        , block l
        , ["}"]
        ]

block :: Block -> [String]
block (ds, ss) = map (replicate 4 ' ' ++) $ vars ++ stmts
  where
    vars = case map snd ds of
        [] -> []
        ds -> map ((++";") . var . (Type "int",) . Just) ds ++ [""]
    stmts = concat $ map stmt (map snd ss)

import' :: String -> [String]
import' name = ["#include <" ++ name ++ ".h>"]

def :: String -> String -> Struct -> Struct -> Block -> [String]
def "h" n as rs _ = 
    [type' (fromStruct rs) ++ " " ++ escape n ++ args as ++ ";"]
def "c" n as rs ss = concat
    [ [type' (fromStruct rs) ++ " " ++ escape n ++ args as ++ " {"]
    , block ss
    , ["}", ""]
    ]
def _ _ _ _ _ = undefined

decl :: String -> Decl -> [String]
decl x = \case
    Import n       -> if x == "h" then import' n else []
    Def n as rs ss -> def x n as rs ss
    _              -> undefined


-- Emit for different files
gaurd :: String -> String
gaurd = (++"_H") . map toUpper . takeBaseName

prefix :: String -> String -> [String]
prefix "h" name =
    [ "#ifndef " ++ gaurd name
    , "#define " ++ gaurd name
    , ""
    ]
prefix "c" name =
    [ "#include \"" ++ (name -<.> "h") ++ "\""
    , ""
    ]
prefix _ _  = undefined

suffix :: String -> String -> [String]
suffix "h" _ = ["", "#endif"]
suffix "c" _ = []
suffix _ _   = undefined

emitExt :: String -> FilePath -> Module -> [String]
emitExt ext name (Module _ ds) = concat
    [ prefix ext name
    , [""]
    , concat $ map (decl ext . snd) ds
    , [""]
    , suffix ext name
    ]

emit :: FilePath -> Module -> [(FilePath, [String])]
emit fp m = map (\x -> (fp -<.> x, emitExt x fp m)) ["h", "c"]

