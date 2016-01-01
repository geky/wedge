module Parse where

import Prelude hiding (lex)
import Control.Applicative
import Data.Char
import Lex
import Rule
import Type
import Base


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


-- Parsing entry point
parse :: String -> Tree
parse cs = run (separated pDecl term) (lex cs) (lexLines cs)

