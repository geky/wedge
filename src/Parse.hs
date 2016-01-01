module Parse where

import Prelude hiding (lex)
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Char
import Lex
import Rule
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


-- Parsing rules
pDecl :: Rule Token Decl
pDecl = rule $ \case
    Symbol "fn":ts -> accept toFn ts
      <*> symbol
      <*> pTuple
      <*> optional (token "->" *> pTuple)
      <*> pBlock
      where toFn name args rets = Fn args rets name
    Symbol "let":ts -> accept Let ts
      <*> symbol
      <*> optional (token "=" *> pExpr)
    _ -> Typed
      <$> pType
      <*> optional symbol
      <*> optional (token "=" *> pExpr)

pExpr :: Rule Token Expr
pExpr = suffix pPreExpr pPostExpr
  where
    pPreExpr = rule $ \case
        Symbol s:ts -> accept (Var s) ts
        Int i:ts    -> accept (IntLit i) ts
        Float f:ts  -> accept (FloatLit f) ts
        String s:ts -> accept (ArrayLit $ map (IntLit . ord) s) ts
        _           -> reject

    pPostExpr = rule $ \case
        Token "(":_ -> flip Call <$
            token "(" <*> delimited pExpr (token ",") <* token ")"
        _           -> reject

pBlock :: Rule Token [Stmt]
pBlock = token "{" *> separated pStmt term <* token "}"

pReturn :: Rule Token Stmt
pReturn = Return <$ token "return" <*> pExpr

pIf :: Rule Token Stmt
pIf = If 
  <$  token "if" <* token "(" <*> pExpr <* token ")"
  <*> pBlock
  <*> (token "else" *> pBlock <|> pure [])

pAssign :: Rule Token Stmt
pAssign = Assign <$> pExpr <* token "=" <*> pExpr

pStmt :: Rule Token Stmt
pStmt
    = pReturn
  <|> pIf
  <|> try (Decl <$> pDecl)
  <|> pExpr <**> (flip Assign <$ token "=" <*> pExpr <|> pure Expr)

parse :: String -> Tree
parse cs = run (separated pDecl term) (lex cs) (lexLines cs)


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

