module Parse where

import Prelude hiding (lex)
import Control.Applicative
import Data.Char
import Lex
import Rule
import Type
import Base


-- Parsing rules
pImport :: Rule Token Decl
pImport = Import <$ token "import" <*> symbol

pDef :: Rule Token Decl
pDef = (\name args rets -> Def args rets name)
  <$  token "def" <*> (symbol <|> op)
  <*> pTuple <*> optional (token "->" *> pTuple)
  <*> pBlock

pLet :: Rule Token Decl
pLet = Let
  <$> (   Left  <$  token "let" <*> pExpr 
      <|> Right <$> ((,) <$> pType <*> (Just <$> symbol)))
      -- TODO fix this in pVar
  <*> optional (token "=" *> pExpr)

pDecl :: Rule Token Decl
pDecl = rule $ \case
    Symbol "import":_ -> pImport
    Symbol "def":_    -> pDef
    _                 -> pLet

pExpr :: Rule Token Expr
pExpr = pSubExpr maxBound

pSubExpr :: Int -> Rule Token Expr
pSubExpr prec = suffix pPreExpr pPostExpr
  where
    pPreExpr = rule $ \case
        Token "(":_  -> token "(" *> pExpr <* token ")"
        Op s prec':_ -> Call (Var s) . pure <$ op <*> pSubExpr prec'
        Symbol s:ts  -> accept (Var s) ts
        Int i:ts     -> accept (IntLit i) ts
        Float f:ts   -> accept (FloatLit f) ts
        String s:ts  -> accept (ArrayLit $ map (IntLit . ord) s) ts
        _            -> reject

    pPostExpr = rule $ \case
        Token "(":_ -> flip Call
          <$  token "("
          <*> delimited pExpr (token ",")
          <*  token ")"
        Token ".":_ -> flip Access
          <$  token "." <*> symbol
        Op s prec':_ | prec > prec' -> (\b a -> Call (Var s) [a,b])
          <$  op <*> pSubExpr prec'
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
          try (Decl <$> pLet <* look term)
      <|> pExpr <**> (flip Assign <$ token "=" <*> pExpr <|> pure Expr)


-- Parsing entry point
parse :: String -> [Decl]
parse cs = run (separated pDecl term) (lex cs) (lexLines cs)

