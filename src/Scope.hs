module Scope where

import Data.Maybe
import Data.Either
import Data.List
import Data.Bifunctor
import Type
import Expr
import Pos
import qualified Parse as P


-- Fully verified and scoped module definitions
-- Decls are removed from Stmts
type Stmt = Positional Stmt'
data Stmt'
  = Expr Expr
  | Assign Expr Expr
  | If Expr [Stmt] [Stmt]
  | While Expr [Stmt]
  | Return Expr
  | Break
  | Continue
  deriving Show

type Import = Positional Import'
data Import' = Import String
  deriving Show

type Decl = Positional Decl'
data Decl' = Decl Type String
  deriving Show

type Def = Positional Def'
data Def' = Def Type String [Decl] [Stmt]
  deriving Show

data Module = Module [Import] [Def]
  deriving Show


-- scope verification and decl lifting
verify, verifyNot :: Pos -> [Decl] -> String -> String
verify p ds s = case find (\(_, Decl _ s') -> s' == s) ds of
    Just _ -> s `seq` error (show ds)
    _      -> errorAt p $ "undefined variable \"" ++ s ++ "\""

verifyNot p ds s = case find (\(_, Decl _ s') -> s' == s) ds of
    Just _ -> errorAt p $ "undefined variable \"" ++ s ++ "\""
    _      -> s

expr :: Pos -> [Decl] -> Expr -> Expr
expr p ds = mapV (verify p ds)

decl :: [Decl] -> P.Stmt -> [Decl]
decl ds (p,s) = case s of
    P.Decl (P.Let (Right (y, Just s)) e) -> 
        verifyNot p ds s `seq` expr p ds <$> e `seq` [(p, Decl y s)]
    P.Decl _     -> errorAt p "unresolved let statement"
    P.Expr e     -> expr p ds e `seq` []
    P.Assign l r -> expr p ds l `seq` expr p ds r `seq` []
    P.Return e   -> expr p ds e `seq` []
    P.If e l r   -> expr p ds e `seq` decls ds (l ++ r)
    P.While e l  -> expr p ds e `seq` decls ds l
    _            -> []

decls :: [Decl] -> [P.Stmt] -> [Decl]
decls ds = concat . tail . scanl decl ds

stmt :: [Decl] -> P.Stmt -> Maybe Stmt
stmt ds (p,s) = (p,) <$> case s of
    P.Decl d     -> Assign (Var s) <$> e
      where (P.Let (Right (_, Just s)) e) = d
    P.Expr e     -> Just $ Expr e
    P.Assign l r -> Just $ Assign l r
    P.Return e   -> Just $ Return e
    P.Break      -> Just $ Break
    P.Continue   -> Just $ Continue
    P.If e l r   -> Just $ If e (stmts ds l) (stmts ds r)
    P.While e l  -> Just $ While e (stmts ds l)

stmts :: [Decl] -> [P.Stmt] -> [Stmt]
stmts ds = catMaybes . map (stmt ds)

block :: [P.Stmt] -> ([Decl], [Stmt])
block s = (decls', stmts')
  where
    decls' = decls [] s
    stmts' = stmts decls' s

topdecl :: P.Decl -> Either Import Def
topdecl (p,d) = bimap (p,) (p,) $ case d of
    P.Def a (Just r) n ss       -> Right $ Def y n `uncurry` block ss
        where y = FuncType a r
    P.Def _ _ _ _               -> errorAt p "unresolved function decl"
    P.Let (Right (y, Just n)) e -> Right $ Def y' n [] s
        where
            y' = FuncType [] (toTuple y)
            s = maybeToList $ (p,) . Return <$> e
    P.Let _ _                   -> errorAt p "unresolved decl"
    P.Import s                  -> Left $ Import s

scope :: P.Tree -> Module
scope = uncurry Module . partitionEithers . map topdecl

