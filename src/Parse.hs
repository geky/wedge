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
expr = call <|> intlit <|> floatlit <|> stringlit <|> var_
  where
    intlit    = IntLit <$> int
    floatlit  = FloatLit <$> float
    stringlit = StringLit <$> string
    var_ = Var <$> sym
    call = Call <$> var_ <* token "(" <*> args <* token ")"
      where args = delimited expr (token ",")
    
    


parse :: [Token] -> [PTree]
parse = run $ separated decl term


-- Emitting definitions
emitTree :: [PTree] -> [String]
emitTree = (concat .) $ map $ \p -> case p of
    Decl y name       -> pure $ emitTypeDecl y name ++ ";"
    FuncDecl y name b -> emitFunc y name b
    Expr e            -> pure $ emitExpr e ++ ";"

emitFunc :: Type -> String -> [PTree] -> [String]
emitFunc (FuncType y z) name b = pure prefix ++ b' ++ pure suffix
  where
    prefix = emitType y ++ " " ++ name ++ emitArgs z ++ " {"
    b' = map (replicate 4 ' ' ++) $ emitTree b
    suffix = "}"
emitFunc _ _ _ = undefined

emitExpr :: Expr -> String
emitExpr = \case
    Call e args -> emitExpr e ++ toArgs args
    Var s       -> s
    IntLit i    -> show i
    FloatLit f  -> show f
    StringLit s -> show s
  where toArgs = (\a -> "(" ++ a ++ ")") . intercalate ", " . map emitExpr
