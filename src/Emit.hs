module Emit where

import Prelude hiding (lex)
import Data.List
import Data.Maybe
import Type
import Base


-- Emitting definitions
emitTree :: Tree -> [String]
emitTree = concat . map emitDecl

emitDecl :: Decl -> [String]
emitDecl = \case
    (Fn a (Just r) name ss) -> concat
        [ [emitType (fromTuple r) ++ " " ++ name ++ emitArgs a ++ " {"]
        , emitBlock ss
        , ["}"]
        ]
    (Typed y name e) -> [emitVar (y, name) ++ init ++ ";"]
      where init = fromMaybe "" ((" = "++) . emitExpr <$> e)
    _ -> undefined

emitBlock :: [Stmt] -> [String]
emitBlock = map (replicate 4 ' ' ++) . concat . map emitStmt

emitStmt :: Stmt -> [String]
emitStmt = \case
    Decl d     -> emitDecl d
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
emitExpr (ArrayLit es) = "{" ++ entries ++ "}"
  where entries = intercalate ", " $ map emitExpr es


-- Emit for different files
emit :: String -> Tree -> String
emit "h" = unlines . emitTree
emit "c" = unlines . emitTree
emit _   = undefined
