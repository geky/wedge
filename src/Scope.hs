module Scope where

import Prelude hiding (error, lookup)
import Data.Foldable
import Data.Bifunctor
import Type
import Expr
import Pos
import qualified Parse as P


-- Fully verified and scoped module definitions
-- Decls are removed from Stmts
type Vars = [(Pos, Var)]

data Stmt
  = Expr Expr
  | Assign Expr Expr
  | If Expr Block Block
  | While Expr Block
  | Return Expr
  | Break
  | Continue
  deriving Show

type Block = (Vars, [(Pos, Stmt)])

data Decl
  = Import String
  | Def String Struct Struct Block
  | Let Expr Expr
  deriving Show

data Module = Module Vars [(Pos, Decl)]
  deriving Show


-- scope verification and decl lifting
lookup :: Var -> Vars -> Maybe (Pos, Var)
lookup v = find ((==v).snd)

let' :: Vars -> Pos -> Expr -> Result Vars
let' vs p = fmap (map (p,)) . traverse letV . toList
  where
    letV v = case lookup v vs of
        Just _  -> error p ("multiply defined variable \"" ++ v ++ "\"")
        Nothing -> ok $ v

expr :: Vars -> Pos -> Expr -> Result Expr
expr vs p = traverse exprV
  where
    exprV v = case lookup v vs of
        Just _  -> ok v
        Nothing -> error p ("undefined variable \"" ++ v ++ "\"")

stmt :: Vars -> Pos -> P.Stmt -> Result (Vars, Stmt)
stmt vs p = \case
    P.Decl (P.Let l r) -> (,) <$> let' vs p l <*> (Assign l <$> expr vs p r)
    s                  -> fmap pure $ case s of
        P.Assign l r -> Assign <$> expr vs p l <*> expr vs p r
        P.Expr e     -> Expr <$> expr vs p e
        P.Return e   -> Return <$> expr vs p e
        P.Break      -> pure Break
        P.Continue   -> pure Continue
        P.If e l r   -> If <$> expr vs p e <*> block vs l <*> block vs r
        P.While e l  -> While <$> expr vs p e <*> block vs l
        _            -> undefined

block :: Vars -> P.Block -> Result Block
block vs' = fmap (fmap reverse) . foldlM scopify ([],[])
  where
    scopify (vs,ss) (p,s) = bimap (++vs) ((:ss).(p,)) <$> stmt (vs++vs') p s

declV :: Vars -> Pos -> P.Decl -> Result Vars
declV vs p = \case
    P.Import _    -> ok []
    P.Def f _ _ _ -> ok [(p,f)]
    P.Let l _     -> let' vs p l
    P.Extern l    -> let' vs p l

decl :: Vars -> Pos -> P.Decl -> Result [Decl]
decl vs p = \case
    P.Import s       -> pure <$> (ok $ Import s)
    P.Def f as rs ss -> pure <$> (Def f as rs <$> block vs' ss)
      where vs' = zip (repeat p) (names as) ++ vs
    P.Let l r        -> pure <$> (Let l <$> expr vs p r)
    P.Extern _       -> ok []

module' :: P.Tree -> Result Module
module' tree = do
    vs <- foldlM (\vs (p,d) -> (++vs) <$> declV vs p d) [] tree
    ds <- mapM (\(p,d) -> map (p,) <$> decl vs p d) tree
    return $ Module vs (concat ds)


-- Scoping entry point
scope :: P.Tree -> Result Module
scope = module'

