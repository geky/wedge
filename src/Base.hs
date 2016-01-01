module Base where

import Type


-- Parse tree definitions
data Expr
    = Call Expr [Expr]
    | Var String
    | IntLit Int
    | FloatLit Double
    | ArrayLit [Expr]
    deriving Show

data Stmt
    = Decl Decl
    | Expr Expr
    | Assign Expr Expr
    | Return Expr
    | If Expr [Stmt] [Stmt]
    deriving Show

data Decl
    = Let String (Maybe Expr)
    | Typed Type (Maybe String) (Maybe Expr)
    | Fn Tuple (Maybe Tuple) String [Stmt]
    deriving Show

type Tree = [Decl]

