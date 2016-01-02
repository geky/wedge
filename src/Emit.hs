module Emit where

import Prelude hiding (lex)
import Data.Char
import Data.List
import Data.Maybe
import Type
import Base


-- Emitting definitions
emitDecl :: String -> Decl -> [String]
emitDecl "h" (Import name) =
    ["#include <" ++ name ++ ".h>"]
emitDecl "h" (Fn a (Just r) name _) =
    [emitType (fromTuple r) ++ " " ++ name ++ emitArgs a ++ ";"]
emitDecl "h" (Typed y name _) =
    ["extern " ++ emitVar (y, name) ++ ";"]
emitDecl "c" (Import _) = []
emitDecl "c" (Fn a (Just r) name ss) = concat
    [ [emitType (fromTuple r) ++ " " ++ name ++ emitArgs a ++ " {"]
    , emitBlock ss
    , ["}"]
    ]
emitDecl "c" (Typed y name e) =
    [emitVar (y, name) ++ init ++ ";"]
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

emitExpr :: Expr -> String
emitExpr (Call e es) = emitExpr e ++ "(" ++ args ++ ")"
  where args = intercalate ", " $ map emitExpr es
emitExpr (Var s)       = s
emitExpr (IntLit i)    = show i
emitExpr (FloatLit f)  = show f
emitExpr (ArrayLit es) = "(char[]){" ++ entries ++ "}"
  where entries = intercalate ", " $ map emitExpr es


-- Emit for different files
emit :: String -> String -> Tree -> String
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

