module Scope (Expr(..), module Scope) where

import Data.Maybe
import Type
import qualified Parse as P
import Parse (Expr(..))


-- Fully verified and scoped module definitions
-- Decls are removed from Stmts
data Stmt
  = Expr Expr
  | Assign Expr Expr
  | If Expr [Stmt] [Stmt]
  | While Expr [Stmt]
  | Return Expr
  | Break
  | Continue
  deriving Show

type Import = String
data Def = Def Type String [(Type, String)] [Stmt]
  deriving Show

data Module = Module [Import] [Def]
  deriving Show


-- scope verification and decl lifting
block :: [P.Stmt] -> ([(Type, String)], [Stmt])
block [] = ([], [])
block (s:ss') = case s of
    P.Decl (P.Let (Right (y, Just s)) e) ->
        ((y, s):ds, maybeToList (Assign (Var s) <$> e) ++ ss)
    P.Decl _ -> error "unresolved let statement"
        -- TODO figure out which fucking line this is on
    P.Expr e     -> (ds, Expr e:ss)
    P.Assign l r -> (ds, Assign l r:ss)
    P.Return e   -> (ds, Return e:ss)
    P.Break      -> (ds, Break:ss)
    P.Continue   -> (ds, Continue:ss)
    P.If p l r   -> (lds ++ rds ++ ds, If p lss rss:ss)
      where
        (lds, lss) = block l
        (rds, rss) = block r
    P.While p l  -> (lds ++ ds, While p lss:ss)
      where
        (lds, lss) = block l
  where
    (ds, ss) = block ss'


scope :: P.Tree -> Module
scope [] = Module [] []
scope (m:ms) = case m of
    P.Def a (Just r) s ss       -> Module is (def:ds)
      where def = uncurry (Def (FuncType a r) s) (block ss)
    P.Def _ _ _ _               -> error "unresolved function decl"
    P.Let (Right (y, Just s)) e -> Module is (def:ds)
      where def = Def (FuncType [] (toTuple y)) s [] (Return <$> maybeToList e)
    P.Let _ _                   -> error "unresolved decl"
    P.Import s                  -> Module (s:is) ds
  where
    Module is ds = scope ms


