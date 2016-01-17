module Scope (Expr(..), module Scope) where

import Data.Maybe
import Type
import qualified Parse as P
import Parse (Expr(..))
import Pos


-- Fully verified and scoped module definitions
-- Decls are removed from Stmts
type Stmt = (Pos, Stmt')
data Stmt'
  = Expr Expr
  | Assign Expr Expr
  | If Expr [Stmt] [Stmt]
  | While Expr [Stmt]
  | Return Expr
  | Break
  | Continue
  deriving Show

type Import = (Pos, Import')
type Import' = String

type Def = (Pos, Def')
data Def' = Def Type String [(Type, String)] [Stmt]
  deriving Show

data Module = Module [Import] [Def]
  deriving Show


-- scope verification and decl lifting
block :: [P.Stmt] -> ([(Type, String)], [Stmt])
block [] = ([], [])
block ((p,s):ss') = case s of
    P.Decl (P.Let (Right (y, Just s)) e) ->
        ((y,s):ds, maybeToList ((\e -> (p, Assign (Var s) e)) <$> e) ++ ss)
    P.Decl _ -> errorAt p "unresolved let statement"
        -- TODO figure out which fucking line this is on
    P.Expr e     -> (ds, (p, Expr e):ss)
    P.Assign l r -> (ds, (p, Assign l r):ss)
    P.Return e   -> (ds, (p, Return e):ss)
    P.Break      -> (ds, (p, Break):ss)
    P.Continue   -> (ds, (p, Continue):ss)
    P.If t l r   -> (lds ++ rds ++ ds, (p, If t lss rss):ss)
      where
        (lds, lss) = block l
        (rds, rss) = block r
    P.While t l  -> (lds ++ ds, (p, While t lss):ss)
      where
        (lds, lss) = block l
  where
    (ds, ss) = block ss'


scope :: P.Tree -> Module
scope [] = Module [] []
scope ((p,m):ms) = case m of
    P.Def a (Just r) s ss       -> Module is ((p,def):ds)
      where def = uncurry (Def (FuncType a r) s) (block ss)
    P.Def _ _ _ _               -> errorAt p "unresolved function decl"
    P.Let (Right (y, Just s)) e -> Module is ((p,def):ds)
      where
        def = Def (FuncType [] (toTuple y)) s [] 
            ((\e -> (p, Return e)) <$> maybeToList e)
    P.Let _ _                   -> errorAt p "unresolved decl"
    P.Import s                  -> Module ((p,s):is) ds
  where
    Module is ds = scope ms


