module Parse where

import Prelude hiding (lex)
import Lex
import Rule
import Type


-- Parse tree definitions
data WExpr
    = WCall WExpr [WExpr]
    | WLit Token
    deriving Show

data WStmt
    = WDeclStmt WDecl
    | WExprStmt WExpr
    deriving Show

data WDecl
    = WLet (Maybe Type) String
    deriving Show

type WTree = [WDecl]



-- Parsing rules
pDecl :: Rule Token WDecl
pDecl = WLet <$> let' <*> symbol
  where
    let' = rule $ \case
        Symbol "let":ts -> accept Nothing ts
        _               -> Just <$> pType
    

parse :: String -> WTree
parse cs = run (separated pDecl term) (lex cs) (lexLines cs)


-- Emitting definitions
emitTree :: WTree -> [String]
emitTree = map emitDecl

emitDecl :: WDecl -> String
emitDecl (WLet (Just y) name) = emitVar (y, Just name) ++ ";"
emitDecl (WLet Nothing _) = undefined

