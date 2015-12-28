module Parse where

import Prelude hiding (lex)
import Control.Applicative
import Data.List
import Lex
import Rule
import Type


-- Parse tree definitions
data Expr
    = Call Expr [Expr]
    | Var String
    | IntLit Int
    | FloatLit Double
    | StringLit String
    deriving Show

data PTree
    = Decl Var
    | FuncDecl Type String [PTree]
    | Expr Expr
    deriving Show

instance Unexpectable PTree where
    xline = pred . length
    xshow = show . head


-- Parsing rules
pDecl :: Rule Token PTree
pDecl = pVar >>= pDeclSuffix

pDeclSuffix :: Var -> Rule Token PTree
pDeclSuffix v = rule $ \case
    Token{ttoken="{"}:_ -> block v
    Token{ttoken="="}:_ -> undefined
    ts                  -> accept (Decl v) ts
  where
    block V{vtype=Just y@(FuncType _ _), vname=Just n} =
        FuncDecl y n <$> pBlock
    block _ = empty

pFunc :: Rule Token PTree
pFunc = toFunc <$> pType <*> symbol <*> pFuncSuffix <*> pBlock
  where toFunc base name suff block = FuncDecl (suff base) name block

pBlock :: Rule Token [PTree]
pBlock = token "{" *> separated pStmt term <* token "}"

pStmt :: Rule Token PTree
pStmt = pDecl <* look (term <|> token "}") <|> Expr <$> pExpr

pExpr :: Rule Token Expr
pExpr = call <|> pInt <|> pFloat <|> pString <|> pVar
  where
    pInt    = IntLit <$> int
    pFloat  = FloatLit <$> float
    pString = StringLit <$> string
    pVar = Var <$> symbol
    call = Call <$> pVar <* token "(" <*> args <* token ")"
      where args = delimited pExpr (token ",")




parse :: [Token] -> [PTree]
parse = run $ separated pDecl term


-- Emitting definitions
emitTree :: [PTree] -> [String]
emitTree = (concat .) $ map $ \case
    Decl v            -> pure $ emitVar v ++ ";"
    FuncDecl y name b -> emitFunc y name b
    Expr e            -> pure $ emitExpr e ++ ";"

emitFunc :: Type -> String -> [PTree] -> [String]
emitFunc f name block = pure prefix ++ block' ++ pure suffix
  where
    FuncType y z = f
    prefix = emitType y ++ " " ++ name ++ emitArgs z ++ " {"
    block' = map (replicate 4 ' ' ++) $ emitTree block
    suffix = "}"

emitExpr :: Expr -> String
emitExpr = \case
    Call e args -> emitExpr e ++ toArgs args
    Var s       -> s
    IntLit i    -> show i
    FloatLit f  -> show f
    StringLit s -> show s
  where toArgs = (\a -> "(" ++ a ++ ")") . intercalate ", " . map emitExpr
