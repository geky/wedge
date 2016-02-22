module Expr where

import Data.Monoid


-- Variables
type Var = String

-- Expression definition
type Expr = Expr' Var
data Expr' v
    = Call (Expr' v) [Expr' v]
    | Access (Expr' v) String
    | Tuple [Expr' v]
    | Var v
    | Int Int
    | Float Double
    deriving (Show, Eq)


-- Operations over variables in expressions
instance Functor Expr' where
    fmap f = \case
        Call e es  -> Call (fmap f e) (map (fmap f) es)
        Access e s -> Access (fmap f e) s
        Tuple es   -> Tuple (map (fmap f) es)
        Var v      -> Var (f v)
        Int n      -> Int n
        Float n    -> Float n

instance Foldable Expr' where
    foldMap f = \case
        Call e es  -> foldMap f e <> foldMap (foldMap f) es
        Access e _ -> foldMap f e
        Tuple es   -> foldMap (foldMap f) es
        Var v      -> f v
        Int _      -> mempty
        Float _    -> mempty

instance Traversable Expr' where
    traverse f = \case
        Call e es  -> Call   <$> traverse f e <*> traverse (traverse f) es
        Access e s -> Access <$> traverse f e <*> pure s
        Tuple es   -> Tuple  <$> traverse (traverse f) es
        Var v      -> Var    <$> f v
        Int n      -> pure $ Int n
        Float n    -> pure $ Float n


-- Basic math operations
exprUnOp
  :: v -> (forall n . Num n => n -> n)
  -> Expr' v -> Expr' v
exprUnOp name op = \case
    Int a   -> Int   $ op a
    Float a -> Float $ op a
    a       -> Call (Var name) [a]

exprBinOp
  :: v -> (forall n . Num n => n -> n -> n)
  -> Expr' v -> Expr' v -> Expr' v
exprBinOp name op = curry $ \case
    (Int a, Int b)     -> Int   $ op a b
    (Int a, Float b)   -> Float $ op (fromIntegral a) b
    (Float a, Int b)   -> Float $ op a (fromIntegral b)
    (Float a, Float b) -> Float $ op a b
    (a, b)             -> Call (Var name) [a, b]

instance Num Expr where
    (+) = exprBinOp "+" (+)
    (-) = exprBinOp "-" (-)
    (*) = exprBinOp "*" (*)

    negate = exprUnOp "-" negate
    abs    = exprUnOp "abs" abs
    signum = exprUnOp "sign" signum

    fromInteger = Int . fromInteger


