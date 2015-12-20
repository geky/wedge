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

instance Unexpectable PTree where
    xline = subtract 1 . length
    xshow = show . head


-- Parsing rules
pDecl :: Rule Token PTree
pDecl = pFunc <|> (uncurry Decl <$> pVar)

pFunc :: Rule Token PTree
pFunc = uncurry FuncDecl <$> pFuncVar <*> pBlock

pBlock :: Rule Token [PTree]
pBlock = bracket $ separated pStmt term

pStmt :: Rule Token PTree
pStmt = pDecl <|> Expr <$> pExpr

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
