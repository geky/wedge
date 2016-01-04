module Emit where

import Prelude hiding (lex)
import Control.Arrow
import Data.Char
import Data.List
import Data.Maybe
import Type
import Scope
  ( Expr(..)
  , Stmt(..)
  , Def(..)
  , Import
  , Module(..)
  )


-- Safe encoding to C symbols
escape :: String -> String
escape = (concat.) $ map $ \case
  '+' -> "add"
  '!' -> "not"
  c   -> pure c


-- Emitting definitions
args :: Tuple -> String
args = (\s -> "("++s++")") . \case
    [] -> "void"
    ys -> intercalate ", " $ map decl ys

members :: Tuple -> String
members = (\s -> "{"++s++"}") . intercalate " " . map ((++";") . decl)

array :: Type -> Maybe Int -> Maybe String -> String
array y n name = type' y ++ name' ++ "[" ++ n' ++ "]"
  where
    name' = fromMaybe "" ((" "++) <$> name)
    n'    = fromMaybe "" (show <$> n)

func :: Tuple -> Tuple -> Maybe String -> String
func x y name = type' (fromTuple y) ++ " (*" ++ name' ++ ")" ++ args x
  where name' = fromMaybe "" name

type' :: Type -> String
type' = \case
    Void            -> "void"
    Type "int"      -> "int"
    Type "uint"     -> "unsigned"
    Type "float"    -> "double"
    Type t          -> t
    StructType ys   -> "struct " ++ members ys
    y               -> decl (y, Nothing)

decl :: (Type, Maybe String) -> String
decl (y, name) = case y of
    ArrayType y n -> type' y ++ name' ++ "[" ++ n' ++ "]"
      where n' = fromMaybe "" (show <$> n)
    FuncType x y  -> type' (fromTuple y) ++ " (*" ++ name' ++ ")" ++ args x
      where name' = fromMaybe "" name
    y             -> type' y ++ name'
  where name' = fromMaybe "" ((" "++) <$> name)

expr :: Expr -> String
expr = \case
    Call e es   -> expr e ++ "(" ++ args ++ ")"
      where args = intercalate ", " $ map expr es
    Access e s  -> expr e ++ "." ++ s
    Var s       -> escape s
    IntLit i    -> show i
    FloatLit f  -> show f
    ArrayLit es -> "(char[]){" ++ entries ++ "}"
      where entries = intercalate ", " $ map expr es

stmt :: Stmt -> [String]
stmt = \case
    Expr e     -> [expr e ++ ";"]
    Assign l r -> [expr l ++ " = " ++ expr r ++ ";"]
    Return e   -> ["return " ++ expr e ++ ";"]
    Break      -> ["break;"]
    Continue   -> ["continue;"]
    If t l []  -> concat
        [ ["if (" ++ expr t ++ ") {"]
        , block [] l
        , ["}"]
        ]
    If t l r   -> concat
        [ ["if (" ++ expr t ++ ") {"]
        , block [] l
        , ["} else {"]
        , block [] r
        , ["}"]
        ]
    While t l  -> concat
        [ ["while (" ++ expr t ++ ") {"]
        , block [] l
        , ["}"]
        ]

block ds ss = map (replicate 4 ' ' ++) $ decls ++ stmts
  where
    decls = case ds of
        [] -> []
        ds -> map ((++";") . decl . second Just) ds ++ [""]
    stmts = concat $ map stmt ss

import' :: Import -> [String]
import' name = ["#include <" ++ name ++ ".h>"]

def :: String -> Def -> [String]
def "h" (Def (FuncType a r) name _ _) = 
    [type' (fromTuple r) ++ " " ++ escape name ++ args a ++ ";"]
def "c" (Def (FuncType a r) name ds ss) = concat
    [ [type' (fromTuple r) ++ " " ++ escape name ++ args a ++ " {"]
    , block ds ss
    , ["}", ""]
    ]
def _ _ = undefined


-- Emit for different files
prefix :: String -> String -> [String]
prefix "h" name =
    [ "#ifndef " ++ map toUpper name ++ "_H"
    , "#define " ++ map toUpper name ++ "_H"
    , ""
    ]
prefix "c" name =
    [ "#include \"" ++ name ++ ".h\""
    , ""
    ]
prefix _ _  = undefined

suffix :: String -> String -> [String]
suffix "h" _ = ["", "#endif"]
suffix "c" _ = []
suffix _ _   = undefined

emit :: String -> String -> Module -> String
emit ext name (Module is ds) = unlines . concat $
    [ prefix ext name
    , if ext == "h" then concat $ map import' is else []
    , [""]
    , concat $ map (def ext) ds
    , [""]
    , suffix ext name
    ]

