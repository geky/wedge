module Expr where

-- Expression definitions
data Expr
    = Call Expr [Expr]
    | Access Expr String
    | Array [Expr]
    | Var String
    | Int Int
    | Float Double
    deriving (Show, Eq)


-- Operations over variables in expressions
mapV :: (String -> String) -> Expr -> Expr
mapV f = \case
    Call e es  -> Call (mapV f e) (map (mapV f) es)
    Access e s -> Access (mapV f e) s
    Array es   -> Array (map (mapV f) es)
    Var s      -> Var (f s)
    Int n      -> Int n
    Float n    -> Float n

foldV :: (a -> String -> a) -> a -> Expr -> a
foldV f a = \case
    Call e es  -> foldl (foldV f) (foldV f a e) es
    Access e _ -> foldV f a e
    Array es   -> foldl (foldV f) a es
    Var s      -> f a s
    Int _      -> a
    Float _    -> a


-- Basic math operations
exprUnOp
  :: String -> (forall n . Num n => n -> n)
  -> Expr -> Expr
exprUnOp name op = \case
    Int a   -> Int $ op a
    Float a -> Float $ op a
    a       -> Call (Var name) [a]

exprBinOp
  :: String -> (forall n . Num n => n -> n -> n)
  -> Expr -> Expr -> Expr
exprBinOp name op = curry $ \case
    (Int a, Int b)     -> Int $ op a b
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


