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
    Symbol "import":ts -> accept Import ts
      <*> symbol
    Symbol "def":ts -> accept toFn ts
      <*> symbol
      <*> pTuple
      <*> optional (token "->" *> pTuple)
      <*> pBlock
      where toFn name args rets = Fn args rets name
    Symbol "let":ts -> accept Let ts
      <*> pExpr
      <*> optional (token "=" *> pExpr)
    _ -> Typed
      <$> pType
      <*> (Just <$> symbol) -- TODO this can't be optional
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
        Token "(":ts -> accept (flip Call) ts 
            <*> delimited pExpr (token ",")
            <* token ")"
        Token ".":ts -> accept (flip Access) ts <*> symbol
        _            -> reject

pBlock :: Rule Token [Stmt]
pBlock = concat <$ token "{" <*> separated pNested term <* token "}"
  where
    pNested = rule $ \case
        Token "{":_ -> pBlock
        _           -> pure <$> pStmt

pIf :: Rule Token Stmt
pIf = If 
  <$  token "if" <* token "(" <*> pExpr <* token ")"
  <*> pBlock
  <*> (token "else" *> pBlock <|> pure [])

pWhile :: Rule Token Stmt
pWhile = While
  <$  token "while" <* token "(" <*> pExpr <* token ")"
  <*> pBlock

pReturn :: Rule Token Stmt
pReturn = Return <$ token "return" <*> pExpr

pBreak :: Rule Token Stmt
pBreak = Break <$ token "break"

pContinue :: Rule Token Stmt
pContinue = Continue <$ token "continue"

pAssign :: Rule Token Stmt
pAssign = Assign <$> pExpr <* token "=" <*> pExpr

pStmt :: Rule Token Stmt
pStmt = rule $ \case
    Symbol "if":_       -> pIf
    Symbol "while":_    -> pWhile
    Symbol "return":_   -> pReturn
    Symbol "break":_    -> pBreak
    Symbol "continue":_ -> pContinue
    _                   ->
          try (Decl <$> pDecl)
      <|> pExpr <**> (flip Assign <$ token "=" <*> pExpr <|> pure Expr)


-- Parsing entry point
parse :: String -> Tree
parse cs = run (separated pDecl term) (lex cs) (lexLines cs)

