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
pDecl = pFunc <|> Decl <$> pVar

pFunc :: Rule Token PTree
pFunc = toFunc <$> pType <*> symbol <*> pFuncSuffix <*> pBlock
  where toFunc base name suff block = FuncDecl (suff base) name block

pBlock :: Rule Token [PTree]
pBlock = bracket $ separated pStmt term

pStmt :: Rule Token PTree
pStmt = pDecl <* look (term <|> token "}")  <|> Expr <$> pExpr

pExpr :: Rule Token Expr
pExpr = call <|> pInt <|> pFloat <|> pString <|> pVar
  where
    pInt    = IntLit <$> int
    pFloat  = FloatLit <$> float
    pString = StringLit <$> string
    pVar = Var <$> symbol
    call = Call <$> pVar <*> paren args
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
