module Emit where

import Prelude hiding (lex)
import Data.Char
import Data.List
import Data.Maybe
import Type
import Base


-- Safe encoding to C symbols
escape :: String -> String
escape = (concat.) $ map $ \case
  '+' -> "add"
  '!' -> "not"
  c   -> pure c


-- Emitting definitions
emitDecl :: String -> Decl -> [String]
emitDecl "h" (Import name) =
    ["#include <" ++ name ++ ".h>"]
emitDecl "h" (Def a (Just r) name _) =
    [emitType (fromTuple r) ++ " " ++ escape name ++ emitArgs a ++ ";"]
emitDecl "h" (Let (Right (y, name)) _) =
    ["extern " ++ emitVar (y, escape <$> name) ++ ";"]
emitDecl "c" (Import _) = []
emitDecl "c" (Def a (Just r) name ss) = concat
    [ [emitType (fromTuple r) ++ " " ++ escape name ++ emitArgs a ++ " {"]
    , emitBlock ss
    , ["}"]
    ]
emitDecl "c" (Let (Right (y, name)) e) =
    [emitVar (y, escape <$> name) ++ init ++ ";"]
  where init = fromMaybe "" ((" = "++) . emitExpr <$> e)
emitDecl _ _ = undefined

emitBlock :: [Stmt] -> [String]
emitBlock = map (replicate 4 ' ' ++) . concat . map emitStmt

emitStmt :: Stmt -> [String]
emitStmt = \case
    Decl d     -> emitDecl "c" d
    Expr e     -> [emitExpr e ++ ";"]
    Assign l r -> [emitExpr l ++ " = " ++ emitExpr r ++ ";"]
    Return e   -> ["return " ++ emitExpr e ++ ";"]
    Break      -> ["break;"]
    Continue   -> ["continue;"]
    If t l []  -> concat
        [ ["if (" ++ emitExpr t ++ ") {"]
        , emitBlock l
        , ["}"]
        ]
    If t l r   -> concat
        [ ["if (" ++ emitExpr t ++ ") {"]
        , emitBlock l
        , ["} else {"]
        , emitBlock r
        , ["}"]
        ]
    While t l  -> concat
        [ ["while (" ++ emitExpr t ++ ") {"]
        , emitBlock l
        , ["}"]
        ]

emitExpr :: Expr -> String
emitExpr (Call e es)   = emitExpr e ++ "(" ++ args ++ ")"
  where args = intercalate ", " $ map emitExpr es
emitExpr (Access e s)  = emitExpr e ++ "." ++ s
emitExpr (Var s)       = escape s
emitExpr (IntLit i)    = show i
emitExpr (FloatLit f)  = show f
emitExpr (ArrayLit es) = "(char[]){" ++ entries ++ "}"
  where entries = intercalate ", " $ map emitExpr es


-- Emit for different files
emit :: String -> String -> [Decl] -> String
emit ext name
  = unlines 
  . (prefix ext ++) . (++ suffix ext)
  . concat . map (emitDecl ext)
  where
    prefix "h" =
        [ "#ifndef " ++ map toUpper name ++ "_H"
        , "#define " ++ map toUpper name ++ "_H"
        , ""
        ]
    prefix "c" =
        [ "#include \"" ++ name ++ ".h\""
        , ""
        ]
    prefix _   = undefined

    suffix "h" = ["", "#endif"]
    suffix "c" = []
    suffix _   = undefined

