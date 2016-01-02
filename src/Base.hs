module Base where

import Type


-- Parse tree definitions
data Expr
    = Call Expr [Expr]
    | Access Expr String
    | Var String
    | IntLit Int
    | FloatLit Double
    | ArrayLit [Expr]
    deriving Show

data Stmt
    = Decl Decl
    | Expr Expr
    | Assign Expr Expr
    | If Expr [Stmt] [Stmt]
    | While Expr [Stmt]
    | Return Expr
    | Break
    | Continue
    deriving Show

data Decl
    = Let Expr (Maybe Expr)
    | Typed Type (Maybe String) (Maybe Expr)
    | Fn Tuple (Maybe Tuple) String [Stmt]
    | Import String
    deriving Show

type Tree = [Decl]


indirect :: Expr -> Bool
indirect (Var _) = True
indirect _       = False


