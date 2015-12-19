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
    = Decl Type String
    | FuncDecl Type String [PTree]
    | Expr Expr
    deriving Show


-- Parsing rules
decl :: Rule Token PTree
decl = func <|> (uncurry Decl <$> var)

func :: Rule Token PTree
func = uncurry FuncDecl <$> funcVar <*> block

block :: Rule Token [PTree]
block = token "{" *> separated stmt term <* token "}"

stmt :: Rule Token PTree
stmt = decl <|> Expr <$> expr

expr :: Rule Token Expr
expr = call <|> intlit <|> floatlit <|> stringlit <|> var
  where
    intlit    = IntLit <$> int
    floatlit  = FloatLit <$> float
    stringlit = StringLit <$> string
    var = Var <$> sym
    call = Call <$> var <* token "(" <*> args <* token ")"
      where args = delimited expr (token ",")
    
    


parse :: [Token] -> [PTree]
parse = run $ separated decl term


-- Emitting definitions
emitTree :: [PTree] -> [String]
emitTree = (concat .) $ map $ \case
    Decl y name       -> pure $ emitTypeDecl y name ++ ";"
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
